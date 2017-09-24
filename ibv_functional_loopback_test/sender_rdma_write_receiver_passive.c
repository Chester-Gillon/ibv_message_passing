/*
 * @file sender_rdma_write_receiver_passive.c
 * @date 3 Sep 2017
 * @author Chester Gillon
 * @details Perform a functional message passing test where:
 *          - Variable size messages are sent in a circular buffer
 *          - The sender and receiver agree on the maximum message size at initialisation,
 *            and at run time the receiver determines the size of the received message from a message header.
 *          - The sender uses RDMA_WRITE
 *          - The receiver is passive, polling memory for message receipt
 *          - The sender uses busy-polling to wait for any previous message transfer to complete
 *
 *          Attempts to optimise the message rate include:
 *          - Sending inline data when the size allows.
 *          - Signalling completion one in every NUM_MESSAGE_BUFFERS when possible.
 *          - Chaining the data and header work requests.
 *
 *          Since a single process is used to send messages between a pair of loop-backed ports on a dual port
 *          Infiniband adapter there is no communication mechanism required to exchange Queue Pair, Memory Region
 *          or Packet Sequence Number details between the sender and receiver (which are in the same process).
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/resource.h>
#include <infiniband/verbs.h>

#include "ibv_utils.h"
#include "ibv_functional_loopback_test_interface.h"

/** To enforce the header being received after the data of each message is available,
 *  the data is sent in one WQE followed by the header */
#define MESSAGE_DATA_WQE_INDEX   0
#define MESSAGE_HEADER_WQE_INDEX 1
#define NUM_WQES_PER_MESSAGE     2

/** The completion queue size required for the message send, allowing for sending data followed by header */
#define TOTAL_MESSAGE_SEND_QUEUE_SIZE (NUM_WQES_PER_MESSAGE * NUM_MESSAGE_BUFFERS)

/** Allocate each sequence number in its own cache line */
typedef struct
{
    uint32_t sequence_number;
    char alignment[CACHE_LINE_SIZE_BYTES - sizeof (uint32_t)];
} aligned_sequnce_number;

/** This structure defines the buffer used by the sender to transmit messages, and receive flow control.
 *  Where this buffer is accessible by the Infiniband device. */
typedef struct
{
    /** The messages which are transmitted */
    test_message transmit_messages[NUM_MESSAGE_BUFFERS];
    /** Used to receive the sequence numbers of the freed messages, to provide flow control */
    aligned_sequnce_number freed_sequence_numbers[NUM_MESSAGE_BUFFERS];
} srwrp_sender_buffer;

/** This structure defines the buffer used by the receiver to receive messages, and transmit flow control.
 *  Where this buffer is accessible by the Infiniband device. */
typedef struct
{
    /** The messages which are received */
    test_message receive_messages[NUM_MESSAGE_BUFFERS];
    /** Used to transmit the sequence numbers of the freed messages, to provide flow control */
    aligned_sequnce_number freed_sequence_numbers[NUM_MESSAGE_BUFFERS];
} srwrp_receiver_buffer;

/** Contains the context to manage one sender message buffer */
typedef struct
{
    /** Contains the scatter-gather entries to transfer the message data then header.
     *  Set at initialisation, and only the size for the message data has to be changed as messages are sent */
    struct ibv_sge send_sges[NUM_WQES_PER_MESSAGE];
    /** Contains the work requests to transfer the message data then header.
     *  Set at initialisation, and do not require modification as messages are sent */
    struct ibv_send_wr send_wrs[NUM_WQES_PER_MESSAGE];
    /** The sequence number for the next message to be sent from this buffer */
    uint32_t next_send_sequence_number;
    /** Set true when srwrp_get_send_buffer() needs to wait for the message buffer to be freed before it can be reused */
    bool await_buffer_freed;
    /** Defines the state of the freed_sequence_numbers entry for this buffer which in updated by the receiver:
     *  - message_not_freed_sequence_number: The previous sent message has not yet been freed.
     *  - message_freed_sequence_number: The previous sent message has been freed.
     *
     *  Any other value indicates an error. */
    uint32_t message_not_freed_sequence_number;
    uint32_t message_freed_sequence_number;
    /** Used to check usage of the message buffer:
     *  - If false in use by the send API functions.
     *  - If true in use by the application for populating the message contents to be sent */
    bool owned_by_application;
} srwrp_message_buffer_send_context;

/** This structure contains the context for the sender of the test messages */
typedef struct
{
    /** The message send buffer which is accessed by this program and the Infiniband device */
    srwrp_sender_buffer *send_buffer;
    /** The Infiniband Memory Region for send_buffer */
    struct ibv_mr *send_mr;
    /** Completion queue for sending the messages */
    struct ibv_cq *message_send_cq;
    /** Queue Pair for sending the messages */
    struct ibv_qp *message_send_qp;
    /** The Packet Sequence Number for message_send_qp */
    uint32_t message_send_psn;
    /** The maximum number of bytes which can be sent inline on message_send_qp */
    uint32_t message_send_qp_max_inline_data;
    /** Contains the sender context for each message buffer */
    srwrp_message_buffer_send_context buffer_contexts[NUM_MESSAGE_BUFFERS];
    /** Used to pass the send buffers to the application */
    api_message_buffer buffers[NUM_MESSAGE_BUFFERS];
    /** The circular buffer index for the next message buffer to pass to the application for populating */
    uint32_t next_send_buffer_index;
    /** Used to control only signalling message send completion on one in every NUM_MESSAGE_BUFFERS messages */
    uint32_t message_send_cq_pacing;
} srwrp_sender_context;

/** Contains the context to manage one receiver message buffer */
typedef struct
{
    /** The scatter-gather entry to transfer the sequence number of a freed message buffer to the sender.
     *  Set at initialisation, and doesn't required modification as messages are freed. */
    struct ibv_sge freed_message_sge;
    /** The work request to transfer the sequence number of a freed message buffer to the sender.
     *  Set at initialisation, and doesn't required modification as messages are freed. */
    struct ibv_send_wr freed_message_wr;
    /** Defines the sequence numbers which appear in the received message header:
     *  - message_not_available_sequence_number: A message is not yet available
     *  -message_available_sequence_number : A message is available
     *
     *  Any other value indicates an error */
    uint32_t message_not_available_sequence_number;
    uint32_t message_available_sequence_number;
    /** Used to check usage of the message buffer:
     *  - If false in use by the receive API functions.
     *  - If true in use by the application for processing the received message contents */
    bool owned_by_application;
} srwrp_message_buffer_receive_context;

/** This structure contains the context for the receiver of the test messages */
typedef struct
{
    /** The message receive buffer which is accessed by this program and the Infiniband device */
    srwrp_receiver_buffer *receive_buffer;
    /** The Infiniband Memory Region for receive_buffer */
    struct ibv_mr *receive_mr;
    /** Completion queue for sending the freed sequence number as part of flow control */
    struct ibv_cq *freed_sequence_number_cq;
    /** Queue Pair for sending the freed sequence number */
    struct ibv_qp *freed_sequence_number_qp;
    /** The Packet Sequence Number for freed_sequence_number_qp */
    uint32_t freed_sequence_number_psn;
    /** The maximum number of bytes which can be sent inline on freed_sequence_number_qp */
    uint32_t freed_sequence_number_qp_max_inline_data;
    /** Contains the receiver context for each message buffer */
    srwrp_message_buffer_receive_context buffer_contexts[NUM_MESSAGE_BUFFERS];
    /** Used to pass the receive buffers to the application */
    api_message_buffer buffers[NUM_MESSAGE_BUFFERS];
    /** The circular buffer index for the next message message to check for message receipt */
    uint32_t next_receive_buffer_index;
    /** Used to control only signalling freed sequence number send completion on one in NUM_MESSAGE_BUFFERS messages */
    uint32_t freed_sequence_number_cq_pacing;
} srwrp_receiver_context;

/**
 * @details Perform the initialisation of the context for the sender of the test messages which creates local
 *          resources without requiring the identity of the Queue Pair on the destination.
 * @param[out] send_context The sender context to initialise
 */
static void srwrp_sender_create_local (srwrp_sender_context *const send_context)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;

    memset (send_context, 0, sizeof (srwrp_sender_context));

    /* Create and register the memory for the send buffers */
    send_context->send_buffer = page_aligned_calloc (1, sizeof (srwrp_sender_buffer));

    send_context->send_mr = ibv_reg_mr (ibv_loopback_device_pd, send_context->send_buffer, sizeof (srwrp_sender_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (send_context->send_mr == NULL)
    {
        perror ("ibv_reg_mrv for send failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the sender.
     * The rationale for the queue sizes are that only one in every NUM_MESSAGE_BUFFERS is signalled for send completion so:
     * - The completion queue only need one entry.
     * - The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *   as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    send_context->message_send_cq = ibv_create_cq (ibv_loopback_device, 1, NULL, NULL, 0);
    if (send_context->message_send_cq == NULL)
    {
        perror ("ibv_create_cq send failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = send_context->message_send_cq;
    qp_init_attr.recv_cq = send_context->message_send_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 2 * TOTAL_MESSAGE_SEND_QUEUE_SIZE;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = false;
    send_context->message_send_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_init_attr);
    if (send_context->message_send_qp == NULL)
    {
        perror ("ibv_create_qp send failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, send_context->message_send_qp, "message_send_qp");
    send_context->message_send_psn = get_random_psn ();
    send_context->message_send_qp_max_inline_data = get_max_inline_data (send_context->message_send_qp);

    /* Transition the sender Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = SOURCE_PORT_NUM;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (send_context->message_send_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    if (rc != 0)
    {
        perror ("ibv_modify_qp message_send_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_INIT, send_context->message_send_qp, "message_send_qp");
}

/**
 * @details Perform the initialisation of the context for the receiver of the test messages which creates local
 *          resources without requiring the identity of the Queue Pair on the source.
 * @param[out] receive_context The receiver context to initialise
 */
static void srwrp_receiver_create_local (srwrp_receiver_context *const receive_context)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;

    memset (receive_context, 0, sizeof (srwrp_receiver_context));

    /* Create and register the memory for the receive buffers */
    receive_context->receive_buffer = page_aligned_calloc (1, sizeof (srwrp_receiver_buffer));

    receive_context->receive_mr = ibv_reg_mr (ibv_loopback_device_pd, receive_context->receive_buffer, sizeof (srwrp_receiver_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (receive_context->receive_mr == NULL)
    {
        perror ("ibv_reg_mrv for receive failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the receiver.
     * The rationale for the queue sizes are that only one in every NUM_MESSAGE_BUFFERS is signalled for send completion so:
     * - The completion queue only need one entry.
     * - The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *   as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    receive_context->freed_sequence_number_cq = ibv_create_cq (ibv_loopback_device, 1, NULL, NULL, 0);
    if (receive_context->freed_sequence_number_cq == NULL)
    {
        perror ("ibv_create_cq receive failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = receive_context->freed_sequence_number_cq;
    qp_init_attr.recv_cq = receive_context->freed_sequence_number_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 2 * NUM_MESSAGE_BUFFERS;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = false;
    receive_context->freed_sequence_number_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_init_attr);
    if (receive_context->freed_sequence_number_qp == NULL)
    {
        perror ("ibv_create_qp receive failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, receive_context->freed_sequence_number_qp, "freed_sequence_number_qp");
    receive_context->freed_sequence_number_psn = get_random_psn ();
    receive_context->freed_sequence_number_qp_max_inline_data = get_max_inline_data (receive_context->freed_sequence_number_qp);

    /* Transition the receiver Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = DESTINATION_PORT_NUM;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (receive_context->freed_sequence_number_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    if (rc != 0)
    {
        perror ("ibv_modify_qp freed_sequence_number_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_INIT, receive_context->freed_sequence_number_qp, "freed_sequence_number_qp");
}

/**
 * @details Perform the initialisation of the context for the sender of test messages for each message buffer
 *          which may be sent.
 *          This sets the Infiniband structures for the send operations using the fixed buffer attributes,
 *          so that only the data size needs to be adjusted as messages are sent.
 * @param[in,out] send_context The sender context to initialise
 * @param[in] receive_context The receiver context containing the "remote" memory region
 */
static void srwrp_sender_initialise_message_buffers (srwrp_sender_context *const send_context,
                                                     const srwrp_receiver_context *const receive_context)
{
    unsigned int buffer_index;

    for (buffer_index = 0; buffer_index < NUM_MESSAGE_BUFFERS; buffer_index++)
    {
        srwrp_message_buffer_send_context *const buffer = &send_context->buffer_contexts[buffer_index];
        struct ibv_sge *const data_sge = &buffer->send_sges[MESSAGE_DATA_WQE_INDEX];
        struct ibv_sge *const header_sge = &buffer->send_sges[MESSAGE_HEADER_WQE_INDEX];
        struct ibv_send_wr *const data_wr = &buffer->send_wrs[MESSAGE_DATA_WQE_INDEX];
        struct ibv_send_wr *const header_wr = &buffer->send_wrs[MESSAGE_HEADER_WQE_INDEX];

        memset (buffer, 0, sizeof (srwrp_message_buffer_send_context));

        /* Set a scatter-gather entry to transmit the maximum message data size, actual length will be set at run time */
        data_sge->lkey = send_context->send_mr->lkey;
        data_sge->addr = (uintptr_t) send_context->send_buffer->transmit_messages[buffer_index].data;
        data_sge->length =  sizeof (send_context->send_buffer->transmit_messages[buffer_index].data);

        /* Set a scatter-gather entry to transmit the fixed size message header */
        header_sge->lkey = send_context->send_mr->lkey;
        header_sge->addr = (uintptr_t) &send_context->send_buffer->transmit_messages[buffer_index].header;
        header_sge->length = sizeof (send_context->send_buffer->transmit_messages[buffer_index].header);

        /* Set the send work request for the message data.
         * The next pointer is set to the header work-request, so that a ibv_post_send() on the the data_wr
         * sends the data followed by the header. */
        data_wr->wr_id = buffer_index;
        data_wr->sg_list = data_sge;
        data_wr->num_sge = 1;
        data_wr->next = header_wr;
        data_wr->opcode = IBV_WR_RDMA_WRITE;
        data_wr->send_flags = 0;
        data_wr->wr.rdma.rkey = receive_context->receive_mr->rkey;
        data_wr->wr.rdma.remote_addr = (uintptr_t) receive_context->receive_mr->addr +
                offsetof (srwrp_receiver_buffer, receive_messages[buffer_index].data);

        /* Set the send work request for the message header */
        header_wr->wr_id = buffer_index;
        header_wr->sg_list = header_sge;
        header_wr->num_sge = 1;
        header_wr->next = NULL;
        header_wr->opcode = IBV_WR_RDMA_WRITE;
        header_wr->send_flags = 0;
        if (sizeof (message_header) <= send_context->message_send_qp_max_inline_data)
        {
            header_wr->send_flags |= IBV_SEND_INLINE;
        }
        header_wr->wr.rdma.rkey = receive_context->receive_mr->rkey;
        header_wr->wr.rdma.remote_addr = (uintptr_t) receive_context->receive_mr->addr +
                offsetof (srwrp_receiver_buffer, receive_messages[buffer_index].header);

        /* Initialise the sequence number management */
        buffer->await_buffer_freed = false;
        buffer->owned_by_application = false;
        buffer->next_send_sequence_number = buffer_index + 1;
        buffer->message_not_freed_sequence_number = 0;
        buffer->message_freed_sequence_number = buffer_index + 1;

        send_context->buffers[buffer_index].message = &send_context->send_buffer->transmit_messages[buffer_index];
        send_context->buffers[buffer_index].buffer_index = buffer_index;
    }

    send_context->next_send_buffer_index = 0;
    send_context->message_send_cq_pacing = 0;
}

/**
 * @details Complete the initialisation of the context for the sender of the test messages by:
 *          - Transition the sender Queue Pair, used to send messages, to the Ready To Send State
 *          - Initialise the message send buffers
 *
 *          This requires the the Queue Pair and memory region for the receive context
 * @param[in,out] send_context The sender context to initialise
 * @param[in] receive_context The receiver context containing the "remote" Queue Pair and memory region
 */
static void srwrp_sender_attach_remote (srwrp_sender_context *const send_context,
                                        const srwrp_receiver_context *const receive_context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    /* Transition the sender Queue Pair to the Ready to Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = ibv_loopback_port_attributes[SOURCE_PORT_NUM].active_mtu;
    qp_attr.dest_qp_num = receive_context->freed_sequence_number_qp->qp_num;
    qp_attr.rq_psn = receive_context->freed_sequence_number_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 12; /* 0.64 milliseconds delay */
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = ibv_loopback_port_attributes[DESTINATION_PORT_NUM].lid;
    qp_attr.ah_attr.sl = DEFAULT_SERVICE_LEVEL;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = SOURCE_PORT_NUM;
    rc = ibv_modify_qp (send_context->message_send_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    if (rc != 0)
    {
        perror ("ibv_modify_qp message_send_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTR, send_context->message_send_qp, "message_send_qp");

    /* Transition the sender Queue Pair to the Ready to Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = send_context->message_send_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7;
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (send_context->message_send_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    if (rc != 0)
    {
        perror ("ibv_modify_qp message_send_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTS, send_context->message_send_qp, "message_send_qp");
    display_qp_capabilities (send_context->message_send_qp, "message_send_qp");

    srwrp_sender_initialise_message_buffers (send_context, receive_context);
}

/**
 * @details Perform the initialisation of the context for the receiver of test messages for each message buffer
 *          which may be received.
 *          This sets the Infiniband structures for the freed sequence number send operations using the fixed buffer attributes.
 * @param[in,out] send_context The receiver context to initialise
 * @param[in] receive_context The sender context containing the "remote" memory region
 */
static void srwrp_receiver_initialise_message_buffers (srwrp_receiver_context *const receive_context,
                                                       const srwrp_sender_context *const send_context)
{
    unsigned int buffer_index;

    for (buffer_index = 0; buffer_index < NUM_MESSAGE_BUFFERS; buffer_index++)
    {
        srwrp_message_buffer_receive_context *const buffer = &receive_context->buffer_contexts[buffer_index];

        /* Set the scatter-gather entry to transmit the freed message sequence number */
        buffer->freed_message_sge.lkey = receive_context->receive_mr->lkey;
        buffer->freed_message_sge.addr =
                (uintptr_t) &receive_context->receive_buffer->freed_sequence_numbers[buffer_index].sequence_number;
        buffer->freed_message_sge.length =
                sizeof (receive_context->receive_buffer->freed_sequence_numbers[buffer_index].sequence_number);

        /* Set the work request to transmit the freed message sequence number */
        buffer->freed_message_wr.wr_id = buffer_index;
        buffer->freed_message_wr.sg_list = &buffer->freed_message_sge;
        buffer->freed_message_wr.num_sge = 1;
        buffer->freed_message_wr.next = NULL;
        buffer->freed_message_wr.opcode = IBV_WR_RDMA_WRITE;
        buffer->freed_message_wr.send_flags = 0;
        if (sizeof (uint32_t) <= receive_context->freed_sequence_number_qp_max_inline_data)
        {
            buffer->freed_message_wr.send_flags |= IBV_SEND_INLINE;
        }
        buffer->freed_message_wr.wr.rdma.rkey = send_context->send_mr->rkey;
        buffer->freed_message_wr.wr.rdma.remote_addr = (uintptr_t) send_context->send_mr->addr +
                offsetof (srwrp_sender_buffer, freed_sequence_numbers[buffer_index].sequence_number);

        /* Initialise the sequence number management */
        buffer->owned_by_application = false;
        buffer->message_not_available_sequence_number = 0;
        buffer->message_available_sequence_number = buffer_index + 1;

        receive_context->buffers[buffer_index].message = &receive_context->receive_buffer->receive_messages[buffer_index];
        receive_context->buffers[buffer_index].buffer_index = buffer_index;
    }

    receive_context->next_receive_buffer_index = 0;
    receive_context->freed_sequence_number_cq_pacing = 0;
}

/**
 * @details Complete the initialisation of the context for the receiver of the test messages by:
 *          - Transition the receiver Queue Pair, used to report freed message buffers, to the Ready To Send State
 *          - Initialise the message receive buffers
 *
 *          This requires the the Queue Pair and memory region for the send context
 * @param[in,out] receive_context The receiver context to initialise
 * @param[in] send_context The sender context containing the "remote" Queue Pair and memory region
 */
static void srwrp_receiver_attach_remote (srwrp_receiver_context *const receive_context,
                                          const srwrp_sender_context *const send_context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    /* Transition the receiver Queue Pair to the Ready to Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = ibv_loopback_port_attributes[DESTINATION_PORT_NUM].active_mtu;
    qp_attr.dest_qp_num = send_context->message_send_qp->qp_num;
    qp_attr.rq_psn = send_context->message_send_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 12; /* 0.64 milliseconds delay */
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = ibv_loopback_port_attributes[SOURCE_PORT_NUM].lid;
    qp_attr.ah_attr.sl = DEFAULT_SERVICE_LEVEL;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = DESTINATION_PORT_NUM;
    rc = ibv_modify_qp (receive_context->freed_sequence_number_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    if (rc != 0)
    {
        perror ("ibv_modify_qp freed_sequence_number_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTR, receive_context->freed_sequence_number_qp, "freed_sequence_number_qp");

    /* Transition the sender Queue Pair to the Ready to Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = receive_context->freed_sequence_number_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7;
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (receive_context->freed_sequence_number_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    if (rc != 0)
    {
        perror ("ibv_modify_qp freed_sequence_number_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTS, receive_context->freed_sequence_number_qp, "freed_sequence_number_qp");
    display_qp_capabilities (receive_context->freed_sequence_number_qp, "freed_sequence_number_qp");

    srwrp_receiver_initialise_message_buffers (receive_context, send_context);
}

/**
 * @brief Free the resources used for the context of a message sender
 * @param[in,out] send_context The context to free the resources for
 */
static void srwrp_sender_finalise (srwrp_sender_context *const send_context)
{
    int rc;

    /* Destroy the queues */
    rc = ibv_destroy_qp (send_context->message_send_qp);
    if (rc != 0)
    {
        perror ("ibv_destroy_qp send failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (send_context->message_send_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cp send failed");
        exit (EXIT_FAILURE);
    }

    /* De-register and free the send buffers */
    rc = ibv_dereg_mr (send_context->send_mr);
    if (rc != 0)
    {
        perror ("ibv_dereg_mr send failed");
        exit (EXIT_FAILURE);
    }
    free (send_context->send_buffer);
}

/**
 * @brief Free the resources used for the context of a message receiver
 * @param[in,out] receive_context The context to free the resources for
 */
static void srwrp_receiver_finalise (srwrp_receiver_context *const receive_context)
{
    int rc;

    /* Destroy the queues */
    rc = ibv_destroy_qp (receive_context->freed_sequence_number_qp);
    if (rc != 0)
    {
        perror ("ibv_destroy_qp receive failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (receive_context->freed_sequence_number_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cq receive failed");
        exit (EXIT_FAILURE);
    }

    /* De-register and free the receive buffers */
    rc = ibv_dereg_mr (receive_context->receive_mr);
    if (rc != 0)
    {
        perror ("ibv_dereg_mr receive failed");
        exit (EXIT_FAILURE);
    }
    free (receive_context->receive_buffer);
}

/**
 * @brief Allocate and initialise the send and receive contexts to be used to transfer test messages
 * @param[out] send_context_out The allocated send context
 * @param[out] receive_context_out The allocated receive context
 */
static void srwrp_initialise (api_send_context *const send_context_out, api_receive_context *const receive_context_out)
{
    srwrp_sender_context *const send_context = cache_line_aligned_alloc (sizeof (srwrp_sender_context));
    srwrp_receiver_context *const receive_context = cache_line_aligned_alloc (sizeof (srwrp_receiver_context));

    srwrp_sender_create_local (send_context);
    srwrp_receiver_create_local (receive_context);
    srwrp_sender_attach_remote (send_context, receive_context);
    srwrp_receiver_attach_remote (receive_context, send_context);

    *send_context_out = (api_send_context) send_context;
    *receive_context_out = (api_receive_context) receive_context;
}

/**
 * @brief Free the sources used for the send and receive contexts used to transfer test messages
 * @param[in] send_context_in The send context to free the resources for
 * @param[in] receive_context_in The receive context to free the resources for
 */
static void srwrp_finalise (api_send_context send_context_in, api_receive_context receive_context_in)
{
    srwrp_sender_context *const send_context = (srwrp_sender_context *) send_context_in;
    srwrp_receiver_context *const receive_context = (srwrp_receiver_context *) receive_context_in;

    srwrp_sender_finalise (send_context);
    srwrp_receiver_finalise (receive_context);
    free (send_context);
    free (receive_context);
}

/**
 * @brief Get a message send buffer to populate
 * @details If the next message buffer is still in use, then waits with a busy-poll for the receiver to indicate
 *          the message has been freed (which also implies the previous send has completed).
 * @param[in,out] send_context_in The send context to get the message buffer for
 * @return Returns a pointer to a message buffer, for which the message field can be populated with a message to send
 */
static api_message_buffer *srwrp_get_send_buffer (api_send_context send_context_in)
{
    srwrp_sender_context *const send_context = (srwrp_sender_context *) send_context_in;
    api_message_buffer *const buffer = &send_context->buffers[send_context->next_send_buffer_index];
    srwrp_message_buffer_send_context *const buffer_context = &send_context->buffer_contexts[buffer->buffer_index];
    const volatile uint32_t *const freed_sequence_number =
            &send_context->send_buffer->freed_sequence_numbers[buffer->buffer_index].sequence_number;

    CHECK_ASSERT (!buffer_context->owned_by_application);

    /* Wait for receiver to indicate the previous message used by this buffer has been freed */
    if (buffer_context->await_buffer_freed)
    {
        bool message_freed = false;

        do
        {
            const uint32_t sampled_sequence_number = *freed_sequence_number;

            if (sampled_sequence_number == buffer_context->message_freed_sequence_number)
            {
                message_freed = true;
            }
            else
            {
                CHECK_ASSERT (sampled_sequence_number == buffer_context->message_not_freed_sequence_number);
            }
        } while (!message_freed);
        buffer_context->message_not_freed_sequence_number = buffer_context->message_freed_sequence_number;
        buffer_context->message_freed_sequence_number = buffer_context->next_send_sequence_number;
        buffer_context->await_buffer_freed = false;
    }

    /* Mark the buffer as being prepared by the application, and advance to the next sender buffer index */
    buffer_context->owned_by_application = true;
    send_context->next_send_buffer_index = (send_context->next_send_buffer_index + 1) % NUM_MESSAGE_BUFFERS;

    return buffer;
}

/**
 * @brief Send a message with the content which has been populated by the caller
 * @details When this function returns the message has been queued for transmission
 * @param[in,out] send_context_in The send context to send the message on
 * @param[in,out] buffer The message buffer to send, which was previously returned by get_send_buffer()
 *                       and which the message to send has been populated by the caller.
 *                       This function will set the buffer->message.header.sequence_number field
 */
static void srwrp_send_message (api_send_context send_context_in, api_message_buffer *const buffer)
{
    srwrp_sender_context *const send_context = (srwrp_sender_context *) send_context_in;
    srwrp_message_buffer_send_context *const buffer_context = &send_context->buffer_contexts[buffer->buffer_index];
    struct ibv_send_wr *bad_wr = NULL;
    int rc;

    CHECK_ASSERT ((buffer_context->owned_by_application) &&
                  (buffer->message->header.message_length <= MAX_MESSAGE_DATA_LEN_BYTES));
    buffer_context->owned_by_application = false;

    /* Complete the message to be sent */
    buffer->message->header.sequence_number = buffer_context->next_send_sequence_number;
    buffer_context->send_sges[MESSAGE_DATA_WQE_INDEX].length = buffer->message->header.message_length;
    if (send_context->message_send_cq_pacing == 0)
    {
        buffer_context->send_wrs[MESSAGE_HEADER_WQE_INDEX].send_flags |= IBV_SEND_SIGNALED;
    }
    else
    {
        buffer_context->send_wrs[MESSAGE_HEADER_WQE_INDEX].send_flags &= ~IBV_SEND_SIGNALED;
    }
    send_context->message_send_cq_pacing++;

    /* Wait for the message send work request completion, which is signalled on one message in every NUM_MESSAGE_BUFFERS */
    if (send_context->message_send_cq_pacing == NUM_MESSAGE_BUFFERS)
    {
        struct ibv_wc wc;
        int num_completions;

        do
        {
            num_completions = ibv_poll_cq (send_context->message_send_cq, 1, &wc);
        } while (num_completions == 0);
        CHECK_ASSERT ((num_completions == 1) && (wc.status == IBV_WC_SUCCESS));
        send_context->message_send_cq_pacing = 0;
    }

    if (buffer_context->send_wrs[MESSAGE_DATA_WQE_INDEX].sg_list->length > 0)
    {
        /* Start the transfer for the data followed by the header */
        if (buffer_context->send_wrs[MESSAGE_DATA_WQE_INDEX].sg_list->length <= send_context->message_send_qp_max_inline_data)
        {
            buffer_context->send_wrs[MESSAGE_DATA_WQE_INDEX].send_flags |= IBV_SEND_INLINE;
        }
        else
        {
            buffer_context->send_wrs[MESSAGE_DATA_WQE_INDEX].send_flags &= ~IBV_SEND_INLINE;
        }
        rc =  ibv_post_send (send_context->message_send_qp, &buffer_context->send_wrs[MESSAGE_DATA_WQE_INDEX], &bad_wr);
        CHECK_ASSERT (rc == 0);
    }
    else
    {
        /* Start the transfer for the header */
        rc =  ibv_post_send (send_context->message_send_qp, &buffer_context->send_wrs[MESSAGE_HEADER_WQE_INDEX], &bad_wr);
        CHECK_ASSERT (rc == 0);
    }

    buffer_context->await_buffer_freed = true;

    /* Advance to the next sequence number for this buffer */
    buffer_context->next_send_sequence_number += NUM_MESSAGE_BUFFERS;
}

/**
 * @brief Wait for a message to be received, using a busy-poll
 * @details The contents of the received message remains valid until the returned buffer is freed by a
 *          call to free_message(). Flow control prevents a received message from being overwritten until freed
 * @param[in,out] receive_context_in The receive context to receive the message on
 * @return Returns a pointer to the received message buffer
 */
static api_message_buffer *srwrp_await_message (api_receive_context receive_context_in)
{
    srwrp_receiver_context *const receive_context = (srwrp_receiver_context *) receive_context_in;
    const uint32_t buffer_index = receive_context->next_receive_buffer_index;
    srwrp_message_buffer_receive_context *const buffer_context = &receive_context->buffer_contexts[buffer_index];
    const volatile uint32_t *const receive_sequence_number =
            &receive_context->receive_buffer->receive_messages[buffer_index].header.sequence_number;
    bool message_available = false;

    /* Wait for the sequence number to indicate the next message is available */
    do
    {
        const uint32_t sampled_sequence_number = *receive_sequence_number;

        if (sampled_sequence_number == buffer_context->message_available_sequence_number)
        {
            message_available = true;
        }
        else
        {
            CHECK_ASSERT (sampled_sequence_number == buffer_context->message_not_available_sequence_number);
        }
    } while (!message_available);

    /* Advance to the next expected receive buffer */
    receive_context->next_receive_buffer_index = (receive_context->next_receive_buffer_index + 1) % NUM_MESSAGE_BUFFERS;

    /* Return the received message */
    buffer_context->owned_by_application = true;
    return &receive_context->buffers[buffer_index];
}

/**
 * @brief Mark a receive message buffer after the received message has been freed
 * @details This implements flow control by indicating to sender that the buffer is now free for another message
 * @param[in,out] receive_context_in The receive context to free the message for
 * @param[in,out] buffer The received message buffer to free
 */
static void srwrp_free_message (api_receive_context receive_context_in, api_message_buffer *const buffer)
{
    srwrp_receiver_context *const receive_context = (srwrp_receiver_context *) receive_context_in;
    srwrp_message_buffer_receive_context *const buffer_context = &receive_context->buffer_contexts[buffer->buffer_index];
    struct ibv_send_wr *bad_wr = NULL;
    int rc;

    CHECK_ASSERT (buffer_context->owned_by_application);

    /* Wait for the freed buffer send work request completion, which is signalled on one message in every NUM_MESSAGE_BUFFERS */
    if (receive_context->freed_sequence_number_cq_pacing == NUM_MESSAGE_BUFFERS)
    {
        int num_completions;
        struct ibv_wc wc;

        do
        {
            num_completions = ibv_poll_cq (receive_context->freed_sequence_number_cq, 1, &wc);
        } while (num_completions == 0);
        CHECK_ASSERT ((num_completions == 1) && (wc.status == IBV_WC_SUCCESS));
        receive_context->freed_sequence_number_cq_pacing = 0;
    }

    /* Transmit the freed sequence number to the sender, to indicate the buffer can be reused */
    receive_context->receive_buffer->freed_sequence_numbers[buffer->buffer_index].sequence_number =
            buffer_context->message_available_sequence_number;
    if (receive_context->freed_sequence_number_cq_pacing == 0)
    {
        buffer_context->freed_message_wr.send_flags |= IBV_SEND_SIGNALED;
    }
    else
    {
        buffer_context->freed_message_wr.send_flags &= ~IBV_SEND_SIGNALED;
    }
    receive_context->freed_sequence_number_cq_pacing++;
    rc = ibv_post_send (receive_context->freed_sequence_number_qp, &buffer_context->freed_message_wr, &bad_wr);
    CHECK_ASSERT (rc == 0);

    /* Advance to the next expected sequence number */
    buffer_context->message_not_available_sequence_number = buffer_context->message_available_sequence_number;
    buffer_context->message_available_sequence_number += NUM_MESSAGE_BUFFERS;
    buffer_context->owned_by_application = false;
}

/**
 * @brief Set the function pointers to transfer messages with the sender using RDMA write and the receive passive
 * @param[out] functions Contains the functions pointers to the message transfer functions in this source file
 */
void sender_rdma_write_receiver_passive_set_functions (message_communication_functions *const functions)
{
    functions->description = "sender using RDMA write and the receiver passive";
    functions->initialise = srwrp_initialise;
    functions->finalise = srwrp_finalise;
    functions->get_send_buffer = srwrp_get_send_buffer;
    functions->send_message = srwrp_send_message;
    functions->await_message = srwrp_await_message;
    functions->free_message = srwrp_free_message;
}
