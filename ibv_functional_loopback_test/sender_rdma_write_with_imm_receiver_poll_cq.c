/*
 * @file sender_rdma_write_with_imm_receiver_poll_cq.c
 * @date 23 Sep 2017
 * @author Chester Gillon
 * @details Perform a functional message passing test where:
 *          - Variable size messages are sent in a circular buffer
 *          - The sender and receiver agree on the maximum message size at initialisation,
 *            and at run time the receiver determines the size of the received message from a message header.
 *          - The sender uses RDMA_WRITE_WITH_IMM.
 *          - The receiver posts receive requests and poll the completion-queue to wait for the message.
 *          - The sender uses busy-polling to wait for any previous message transfer to complete.
 *
 *          Attempts to optimise the message rate include:
 *          - Sending inline data when the size allows.
 *          - Signalling completion one in every NUM_MESSAGE_BUFFERS when possible.
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

/** This structure defines the buffer used by the sender to transmit messages.
 *  The flow control is received via the immediate data value, and so doesn't appear in this buffer.
 *  Where this buffer is accessible by the Infiniband device. */
typedef struct
{
    /** The messages which are transmitted */
    test_message transmit_messages[NUM_MESSAGE_BUFFERS];
} srwirpcq_sender_buffer;

/** This structure defines the buffer used by the receiver to receive messages.
 *  The flow control is transmitted via the immediate data value, and so doesn't appear in this buffer.
 *  Where this buffer is accessible by the Infiniband device. */
typedef struct
{
    /** The messages which are received */
    test_message receive_messages[NUM_MESSAGE_BUFFERS];
} srwirpcq_receiver_buffer;

/** Contains the context to manage one sender message buffer */
typedef struct
{
    /** Contains the scatter-gather entry to transfer the message header+data.
     *  Set at initialisation, and only the size for the message header+data has to be changed as messages are sent. */
    struct ibv_sge send_sge;
    /** Contains the work request to transfer the message header+data.
     *  Set at initialisation. The following flags are changed as messages are sent:
     *    IBV_SEND_SIGNALED : Set for one out of every NUM_MESSAGE_BUFFERS as an optimisation to minimise the overhead
     *                        of polling for completion.
     *                        Since flow-control is used to indicate that the receiver has freed a message buffer,
     *                        when have received an indicate the buffer has been freed that implicitly means the
     *                        message send transfer has completed.
     *    IBV_SEND_INLINE : Set if the number of bytes header+data is small enough to be sent as inline, as an optimisation. */
    struct ibv_send_wr send_wr;
    /** Contains the work-request used to receive immediate data for when the buffer has been freed by the receiver */
    struct ibv_recv_wr freed_buffer_wr;
    /** The sequence number for the next message to be sent from this buffer */
    uint32_t next_send_sequence_number;
    /** Indicates when the buffer in the receiver is free, allowing a message to be sent:
     *  - Initialised to true.
     *  - Cleared when a message has been sent.
     *  - Set when the receiver sends a freed buffer index via immediate data. */
    bool buffer_free;
    /** Used to check usage of the message buffer:
     *  - If false in use by the send API functions.
     *  - If true in use by the application for populating the message contents to be sent */
    bool owned_by_application;
} srwirpcq_message_buffer_send_context;

/** This structure contains the context for the sender of the test messages */
typedef struct
{
    /** The message send buffer which is accessed by this program and the Infiniband device */
    srwirpcq_sender_buffer *send_buffer;
    /** The Infiniband Memory Region for send_buffer */
    struct ibv_mr *send_mr;
    /** Completion queue for sending the messages */
    struct ibv_cq *message_send_cq;
    /** Completion queue for receiving the freed buffer indices, which is signalled for one in NUM_MESSAGE_BUFFERS messages */
    struct ibv_cq *freed_message_cq;
    /** Queue Pair for sending the messages, and receiving the freed buffer indices */
    struct ibv_qp *sender_qp;
    /** The Packet Sequence Number for sender_qp */
    uint32_t message_send_psn;
    /** The maximum number of bytes which can be sent inline on sender_qp */
    uint32_t sender_qp_max_inline_data;
    /** Contains the sender context for each message buffer */
    srwirpcq_message_buffer_send_context buffer_contexts[NUM_MESSAGE_BUFFERS];
    /** Used to pass the send buffers to the application */
    api_message_buffer buffers[NUM_MESSAGE_BUFFERS];
    /** The circular buffer index for the next message buffer to pass to the application for populating */
    uint32_t next_send_buffer_index;
    /** Used to control only signalling message send completion on one in every NUM_MESSAGE_BUFFERS messages */
    uint32_t message_send_cq_pacing;
    /** Used to poll for all freed buffer indices completions at once */
    struct ibv_wc freed_message_completions[NUM_MESSAGE_BUFFERS];
} srwirpcq_sender_context;

/** Contains the context to manage one receiver message buffer */
typedef struct
{
    /** The work request used to receive immediate data for when the buffer has been written to with a message */
    struct ibv_recv_wr message_available_wr;
    /** The work request to transfer the the index of a freed message buffer to the sender (via immediate data).
     *  Set at initialisation, and doesn't required modification as messages are freed. */
    struct ibv_send_wr freed_buffer_wr;
    /** Defines the next expected sequence number for the receive message header, which is checked after
     *  freed_buffer_wr has completed.
     *  Any other value indicates an error. */
    uint32_t message_available_sequence_number;
    /** When true a message is available for returning to the application.
     *  As an optimisation await_message() can remove multiple available buffer indices from the completion queue
     *  and therefore this flag can be set on later buffers. */
    bool message_available;
    /** Used to check usage of the message buffer:
     *  - If false in use by the receive API functions.
     *  - If true in use by the application for processing the received message contents */
    bool owned_by_application;
} srwirpcq_message_buffer_receive_context;

/** This structure contains the context for the receiver of the test messages */
typedef struct
{
    /** The message receive buffer which is accessed by this program and the Infiniband device */
    srwirpcq_receiver_buffer *receive_buffer;
    /** The Infiniband Memory Region for receive_buffer */
    struct ibv_mr *receive_mr;
    /** Completion queue for receiving the buffer index for an available receive message */
    struct ibv_cq *message_available_cq;
    /** Completion queue for sending the freed buffer index number as part of flow control,
     *  which is signalled for one in NUM_MESSAGE_BUFFERS messages */
    struct ibv_cq *freed_buffer_cq;
    /** Queue Pair for receiving the available message indices, and sending the freed buffer indices */
    struct ibv_qp *receiver_qp;
    /** The Packet Sequence Number for receiver_qp */
    uint32_t freed_buffer_psn;
    /** Contains the receiver context for each message buffer */
    srwirpcq_message_buffer_receive_context buffer_contexts[NUM_MESSAGE_BUFFERS];
    /** Used to pass the receive buffers to the application */
    api_message_buffer buffers[NUM_MESSAGE_BUFFERS];
    /** The circular buffer index for the next message message to check for message receipt */
    uint32_t next_receive_buffer_index;
    /** Used to control only signalling freed buffer index send completion on one in NUM_MESSAGE_BUFFERS messages */
    uint32_t freed_buffer_cq_pacing;
    /** Used to poll for all available message buffer indices completions at once */
    struct ibv_wc message_available_completions[NUM_MESSAGE_BUFFERS];
} srwirpcq_receiver_context;

/**
 * @details Perform the initialisation of the context for the sender of the test messages which creates local
 *          resources without requiring the identity of the Queue Pair on the destination.
 * @param[out] send_context The sender context to initialise
 */
static void srwirpcq_sender_create_local (srwirpcq_sender_context *const send_context)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;

    memset (send_context, 0, sizeof (srwirpcq_sender_context));

    /* Create and register the memory for the send buffers */
    send_context->send_buffer = page_aligned_calloc (1, sizeof (srwirpcq_sender_buffer));

    send_context->send_mr = ibv_reg_mr (ibv_loopback_device_pd, send_context->send_buffer, sizeof (srwirpcq_sender_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (send_context->send_mr == NULL)
    {
        perror ("ibv_reg_mrv for send failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the sender.
     * The message send Completion Queue is only signalled for one buffer out of NUM_MESSAGE_BUFFERS as an optimisation.
     * The freed message Completion Queue is signalled for each buffer to detect as soon as a buffer is freed.
     *
     * The rationale for the queue sizes are that only one in every NUM_MESSAGE_BUFFERS is signalled for send completion so:
     * - The completion queue only need one entry.
     * - The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *   as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    send_context->message_send_cq = ibv_create_cq (ibv_loopback_device, 1, NULL, NULL, 0);
    if (send_context->message_send_cq == NULL)
    {
        perror ("ibv_create_cq message_send_cq failed");
        exit (EXIT_FAILURE);
    }

    send_context->freed_message_cq = ibv_create_cq (ibv_loopback_device, NUM_MESSAGE_BUFFERS, NULL, NULL, 0);
    if (send_context->freed_message_cq == NULL)
    {
        perror ("ibv_create_cq freed_message_cq failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = send_context->message_send_cq;
    qp_init_attr.recv_cq = send_context->freed_message_cq;
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 2 * NUM_MESSAGE_BUFFERS;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = NUM_MESSAGE_BUFFERS;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = false;
    send_context->sender_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_init_attr);
    if (send_context->sender_qp == NULL)
    {
        perror ("ibv_create_qp send failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, send_context->sender_qp, "sender_qp");
    send_context->message_send_psn = get_random_psn ();
    send_context->sender_qp_max_inline_data = get_max_inline_data (send_context->sender_qp);

    /* Transition the sender Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = SOURCE_PORT_NUM;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (send_context->sender_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    if (rc != 0)
    {
        perror ("ibv_modify_qp message_send_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_INIT, send_context->sender_qp, "sender_qp");
}

/**
 * @details Perform the initialisation of the context for the receiver of the test messages which creates local
 *          resources without requiring the identity of the Queue Pair on the source.
 * @param[out] receive_context The receiver context to initialise
 */
static void srwirpcq_receiver_create_local (srwirpcq_receiver_context *const receive_context)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;

    memset (receive_context, 0, sizeof (srwirpcq_receiver_context));

    /* Create and register the memory for the receive buffers */
    receive_context->receive_buffer = page_aligned_calloc (1, sizeof (srwirpcq_receiver_buffer));

    receive_context->receive_mr = ibv_reg_mr (ibv_loopback_device_pd, receive_context->receive_buffer,
            sizeof (srwirpcq_receiver_buffer), IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (receive_context->receive_mr == NULL)
    {
        perror ("ibv_reg_mrv for receive failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the receiver
     * The message available Completion Queue is signalled for each buffer to detect as soon as a message is available.
     * The freed buffer Completion Queue is only signalled for one buffer out of NUM_MESSAGE_BUFFERS as an optimisation.
     *
     * The rationale for the queue sizes are that only one in every NUM_MESSAGE_BUFFERS is signalled for send completion so:
     * - The completion queue only need one entry.
     * - The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *   as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    receive_context->message_available_cq = ibv_create_cq (ibv_loopback_device, NUM_MESSAGE_BUFFERS, NULL, NULL, 0);
    if (receive_context->message_available_cq == NULL)
    {
        perror ("ibv_create_cq message_available_cq failed");
        exit (EXIT_FAILURE);
    }

    receive_context->freed_buffer_cq = ibv_create_cq (ibv_loopback_device, 1, NULL, NULL, 0);
    if (receive_context->freed_buffer_cq == NULL)
    {
        perror ("ibv_create_cq freed_buffer_cq failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = receive_context->freed_buffer_cq;
    qp_init_attr.recv_cq = receive_context->message_available_cq;
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 2 * NUM_MESSAGE_BUFFERS;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = NUM_MESSAGE_BUFFERS;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = false;
    receive_context->receiver_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_init_attr);
    if (receive_context->receiver_qp == NULL)
    {
        perror ("ibv_create_qp receiver_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, receive_context->receiver_qp, "receiver_qp");
    receive_context->freed_buffer_psn = get_random_psn ();

    /* Transition the receiver Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = DESTINATION_PORT_NUM;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (receive_context->receiver_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    if (rc != 0)
    {
        perror ("ibv_modify_qp receiver_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_INIT, receive_context->receiver_qp, "receiver_qp");
}

/**
 * @details Perform the initialisation of the context for the sender of test messages for each message buffer
 *          which may be sent.
 *          This sets the Infiniband structures for the send operations using the fixed buffer attributes,
 *          so that only the data size related fields to be adjusted as messages are sent.
 * @param[in,out] send_context The sender context to initialise
 * @param[in] receive_context The receiver context containing the "remote" memory region
 */
static void srwirpcq_sender_initialise_message_buffers (srwirpcq_sender_context *const send_context,
                                                        const srwirpcq_receiver_context *const receive_context)
{
    unsigned int buffer_index;

    for (buffer_index = 0; buffer_index < NUM_MESSAGE_BUFFERS; buffer_index++)
    {
        srwirpcq_message_buffer_send_context *const buffer = &send_context->buffer_contexts[buffer_index];

        memset (buffer, 0, sizeof (srwirpcq_message_buffer_send_context));

        /* Set a scatter-gather entry to transmit the header + maximum message data size, actual length will be set at run time */
        buffer->send_sge.lkey = send_context->send_mr->lkey;
        buffer->send_sge.addr = (uintptr_t) &send_context->send_buffer->transmit_messages[buffer_index];
        buffer->send_sge.length = sizeof (send_context->send_buffer->transmit_messages[buffer_index]);

        /* Set the send work request for the message header+data.
         * The wr_id is set to the buffer_index to handle calls to srwirpcq_send_message() out-of-order */
        buffer->send_wr.wr_id = buffer_index;
        buffer->send_wr.sg_list = &buffer->send_sge;
        buffer->send_wr.num_sge = 1;
        buffer->send_wr.next = NULL;
        buffer->send_wr.opcode = IBV_WR_RDMA_WRITE_WITH_IMM;
        buffer->send_wr.send_flags = 0;
        buffer->send_wr.imm_data = buffer_index;
        buffer->send_wr.wr.rdma.rkey = receive_context->receive_mr->rkey;
        buffer->send_wr.wr.rdma.remote_addr = (uintptr_t) receive_context->receive_mr->addr +
                offsetof (srwirpcq_receiver_buffer, receive_messages[buffer_index]);

        /* Set the receive work request for the freed buffer indices as immediate data */
        buffer->freed_buffer_wr.wr_id = 0;
        buffer->freed_buffer_wr.sg_list = NULL;
        buffer->freed_buffer_wr.num_sge = 0;
        buffer->freed_buffer_wr.next = NULL;

        /* Initialise the sequence number management */
        buffer->buffer_free = true;
        buffer->owned_by_application = false;
        buffer->next_send_sequence_number = buffer_index + 1;

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
static void srwirpcq_sender_attach_remote (srwirpcq_sender_context *const send_context,
                                           const srwirpcq_receiver_context *const receive_context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    /* Transition the sender Queue Pair to the Ready to Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = ibv_loopback_port_attributes[SOURCE_PORT_NUM].active_mtu;
    qp_attr.dest_qp_num = receive_context->receiver_qp->qp_num;
    qp_attr.rq_psn = receive_context->freed_buffer_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 12; /* 0.64 milliseconds delay */
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = ibv_loopback_port_attributes[DESTINATION_PORT_NUM].lid;
    qp_attr.ah_attr.sl = DEFAULT_SERVICE_LEVEL;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = SOURCE_PORT_NUM;
    rc = ibv_modify_qp (send_context->sender_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    if (rc != 0)
    {
        perror ("ibv_modify_qp sender_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTR, send_context->sender_qp, "sender_qp");

    /* Transition the sender Queue Pair to the Ready to Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = send_context->message_send_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7;
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (send_context->sender_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    if (rc != 0)
    {
        perror ("ibv_modify_qp sender_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTS, send_context->sender_qp, "sender_qp");
    display_qp_capabilities (send_context->sender_qp, "sender_qp");

    srwirpcq_sender_initialise_message_buffers (send_context, receive_context);
}

/**
 * @details Perform the initialisation of the context for the receiver of test messages for each message buffer
 *          which may be received.
 *          This sets the Infiniband structures for the:
 *          - Receiving the indices of available messages, via immediate data.
 *          - Freed index number send operations using the fixed buffer attributes.
 * @param[in,out] send_context The receiver context to initialise
 * @param[in] receive_context The sender context containing the "remote" memory region
 */
static void srwirpcq_receiver_initialise_message_buffers (srwirpcq_receiver_context *const receive_context,
                                                          const srwirpcq_sender_context *const send_context)
{
    unsigned int buffer_index;
    int rc;

    for (buffer_index = 0; buffer_index < NUM_MESSAGE_BUFFERS; buffer_index++)
    {
        srwirpcq_message_buffer_receive_context *const buffer = &receive_context->buffer_contexts[buffer_index];
        struct ibv_recv_wr *bad_wr = NULL;

        /* Set the work request used to receive the buffer index of available messages via immediate data */
        buffer->message_available_wr.wr_id = buffer_index;
        buffer->message_available_wr.sg_list = NULL;
        buffer->message_available_wr.num_sge = 0;
        buffer->message_available_wr.next = NULL;

        /* Set the work request used to transmit the freed buffer index as immediate data */
        buffer->freed_buffer_wr.wr_id = buffer_index;
        buffer->freed_buffer_wr.sg_list = NULL;
        buffer->freed_buffer_wr.num_sge = 0;
        buffer->freed_buffer_wr.next = NULL;
        buffer->freed_buffer_wr.opcode = IBV_WR_RDMA_WRITE_WITH_IMM;
        buffer->freed_buffer_wr.send_flags = 0;
        buffer->freed_buffer_wr.imm_data = buffer_index;
        buffer->freed_buffer_wr.wr.rdma.rkey = send_context->send_mr->rkey;
        buffer->freed_buffer_wr.wr.rdma.remote_addr = (uintptr_t) send_context->send_mr->addr;

        /* Initialise the sequence number management */
        buffer->message_available = false;
        buffer->owned_by_application = false;
        buffer->message_available_sequence_number = buffer_index + 1;

        receive_context->buffers[buffer_index].message = &receive_context->receive_buffer->receive_messages[buffer_index];
        receive_context->buffers[buffer_index].buffer_index = buffer_index;

        /* Post the work request to receive the available message */
        rc = ibv_post_recv (receive_context->receiver_qp, &buffer->message_available_wr, &bad_wr);
        if (rc != 0)
        {
            perror ("ibv_post_recv message_available_wr failed");
            exit (EXIT_FAILURE);
        }
    }

    receive_context->next_receive_buffer_index = 0;
    receive_context->freed_buffer_cq_pacing = 0;
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
static void srwirpcq_receiver_attach_remote (srwirpcq_receiver_context *const receive_context,
                                             const srwirpcq_sender_context *const send_context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    /* Transition the receiver Queue Pair to the Ready to Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = ibv_loopback_port_attributes[DESTINATION_PORT_NUM].active_mtu;
    qp_attr.dest_qp_num = send_context->sender_qp->qp_num;
    qp_attr.rq_psn = send_context->message_send_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 12; /* 0.64 milliseconds delay */
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = ibv_loopback_port_attributes[SOURCE_PORT_NUM].lid;
    qp_attr.ah_attr.sl = DEFAULT_SERVICE_LEVEL;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = DESTINATION_PORT_NUM;
    rc = ibv_modify_qp (receive_context->receiver_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    if (rc != 0)
    {
        perror ("ibv_modify_qp receiver_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTR, receive_context->receiver_qp, "receiver_qp");

    /* Transition the sender Queue Pair to the Ready to Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = receive_context->freed_buffer_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7;
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (receive_context->receiver_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    if (rc != 0)
    {
        perror ("ibv_modify_qp receiver_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RTS, receive_context->receiver_qp, "receiver_qp");
    display_qp_capabilities (receive_context->receiver_qp, "receiver_qp");

    srwirpcq_receiver_initialise_message_buffers (receive_context, send_context);
}

/**
 * @brief Allocate and initialise the send and receive contexts to be used to transfer test messages
 * @param[out] send_context_out The allocated send context
 * @param[out] receive_context_out The allocated receive context
 */
static void srwirpcq_initialise (api_send_context *const send_context_out, api_receive_context *const receive_context_out)
{
    srwirpcq_sender_context *const send_context = cache_line_aligned_alloc (sizeof (srwirpcq_sender_context));
    srwirpcq_receiver_context *const receive_context = cache_line_aligned_alloc (sizeof (srwirpcq_receiver_context));

    srwirpcq_sender_create_local (send_context);
    srwirpcq_receiver_create_local (receive_context);
    srwirpcq_sender_attach_remote (send_context, receive_context);
    srwirpcq_receiver_attach_remote (receive_context, send_context);

    *send_context_out = (api_send_context) send_context;
    *receive_context_out = (api_receive_context) receive_context;
}

/**
 * @brief Free the resources used for the context of a message sender
 * @param[in,out] send_context The context to free the resources for
 */
static void srwirpcq_sender_finalise (srwirpcq_sender_context *const send_context)
{
    int rc;

    /* Destroy the queues */
    rc = ibv_destroy_qp (send_context->sender_qp);
    if (rc != 0)
    {
        perror ("ibv_destroy_qp sender_qp failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (send_context->message_send_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cp message_send_cq failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (send_context->freed_message_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cp freed_message_cq failed");
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
static void srwirpcq_receiver_finalise (srwirpcq_receiver_context *const receive_context)
{
    int rc;

    /* Destroy the queues */
    rc = ibv_destroy_qp (receive_context->receiver_qp);
    if (rc != 0)
    {
        perror ("ibv_destroy_qp receiver_qp failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (receive_context->freed_buffer_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cq freed_buffer_cq failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (receive_context->message_available_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cq message_available_cq failed");
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
 * @brief Free the sources used for the send and receive contexts used to transfer test messages
 * @param[in] send_context_in The send context to free the resources for
 * @param[in] receive_context_in The receive context to free the resources for
 */
static void srwirpcq_finalise (api_send_context send_context_in, api_receive_context receive_context_in)
{
    srwirpcq_sender_context *const send_context = (srwirpcq_sender_context *) send_context_in;
    srwirpcq_receiver_context *const receive_context = (srwirpcq_receiver_context *) receive_context_in;

    srwirpcq_sender_finalise (send_context);
    srwirpcq_receiver_finalise (receive_context);
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
static api_message_buffer *srwirpcq_get_send_buffer (api_send_context send_context_in)
{
    srwirpcq_sender_context *const send_context = (srwirpcq_sender_context *) send_context_in;
    api_message_buffer *const buffer = &send_context->buffers[send_context->next_send_buffer_index];
    srwirpcq_message_buffer_send_context *const buffer_context = &send_context->buffer_contexts[buffer->buffer_index];
    srwirpcq_message_buffer_send_context *freed_context;
    int num_completions;
    int completion_index;

    CHECK_ASSERT (!buffer_context->owned_by_application);

    /* Wait for the receiver to indicate the previous message sent from this buffer has been freed.
     * The loop allows buffers to be freed out-of-order. */
    while (!buffer_context->buffer_free)
    {
        do
        {
            num_completions = ibv_poll_cq (send_context->freed_message_cq, NUM_MESSAGE_BUFFERS,
                    send_context->freed_message_completions);
        } while (num_completions == 0);
        CHECK_ASSERT ((num_completions >= 1) && (num_completions <= NUM_MESSAGE_BUFFERS));

        for (completion_index = 0; completion_index < num_completions; completion_index++)
        {
            const struct ibv_wc *const wc = &send_context->freed_message_completions[completion_index];
            const uint32_t freed_buffer_index = wc->imm_data;

            CHECK_ASSERT ((wc->status == IBV_WC_SUCCESS) && (freed_buffer_index < NUM_MESSAGE_BUFFERS));
            freed_context = &send_context->buffer_contexts[freed_buffer_index];
            CHECK_ASSERT (!freed_context->buffer_free);
            freed_context->buffer_free = true;
        }
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
static void srwirpcq_send_message (api_send_context send_context_in, api_message_buffer *const buffer)
{
    srwirpcq_sender_context *const send_context = (srwirpcq_sender_context *) send_context_in;
    srwirpcq_message_buffer_send_context *const buffer_context = &send_context->buffer_contexts[buffer->buffer_index];
    struct ibv_recv_wr *bad_recv_wr = NULL;
    struct ibv_send_wr *bad_send_wr = NULL;
    int send_flags;
    int rc;

    CHECK_ASSERT ((buffer_context->owned_by_application) &&
                  (buffer_context->buffer_free) &&
                  (buffer->message->header.message_length <= MAX_MESSAGE_DATA_LEN_BYTES));
    buffer_context->owned_by_application = false;

    /* Post the work request to receive the buffer freed index, when the receiver frees the message */
    buffer_context->buffer_free = false;
    rc = ibv_post_recv (send_context->sender_qp, &buffer_context->freed_buffer_wr, &bad_recv_wr);
    CHECK_ASSERT (rc == 0);

    /* Complete the message to be sent */
    buffer->message->header.sequence_number = buffer_context->next_send_sequence_number;
    buffer_context->send_sge.length = sizeof (message_header) + buffer->message->header.message_length;
    send_flags = 0;
    if (send_context->message_send_cq_pacing == 0)
    {
        send_flags |= IBV_SEND_SIGNALED;
    }
    if (buffer_context->send_sge.length <= send_context->sender_qp_max_inline_data)
    {
        send_flags |= IBV_SEND_INLINE;
    }
    buffer_context->send_wr.send_flags = send_flags;
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

    /* Start the transfer for the message */
    rc = ibv_post_send (send_context->sender_qp, &buffer_context->send_wr, &bad_send_wr);
    CHECK_ASSERT (rc == 0);

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
static api_message_buffer *srwirpcq_await_message (api_receive_context receive_context_in)
{
    srwirpcq_receiver_context *const receive_context = (srwirpcq_receiver_context *) receive_context_in;
    const uint32_t buffer_index = receive_context->next_receive_buffer_index;
    srwirpcq_message_buffer_receive_context *const buffer_context = &receive_context->buffer_contexts[buffer_index];

    /* Wait for the next message buffer to have an available message.
     * The loop extracts all available buffer indices from the completion queue as an optimisation */
    while (!buffer_context->message_available)
    {
        int num_completions;
        int completion_index;

        do
        {
            num_completions = ibv_poll_cq (receive_context->message_available_cq, NUM_MESSAGE_BUFFERS,
                    receive_context->message_available_completions);
        } while (num_completions == 0);
        CHECK_ASSERT ((num_completions >= 1) && (num_completions <= NUM_MESSAGE_BUFFERS));

        for (completion_index = 0; completion_index < num_completions; completion_index++)
        {
            const struct ibv_wc *const wc = &receive_context->message_available_completions[completion_index];
            const uint32_t available_buffer_index = wc->imm_data;
            srwirpcq_message_buffer_receive_context *available_context;
            const message_header *header;

            CHECK_ASSERT ((wc->status == IBV_WC_SUCCESS) && (available_buffer_index < NUM_MESSAGE_BUFFERS));
            available_context = &receive_context->buffer_contexts[available_buffer_index];
            header = &receive_context->receive_buffer->receive_messages[available_buffer_index].header;
            CHECK_ASSERT (!available_context->message_available &&
                    (wc->byte_len == (sizeof (message_header) + header->message_length)) &&
                    (header->sequence_number == available_context->message_available_sequence_number));
            available_context->message_available = true;
            available_context->message_available_sequence_number += NUM_MESSAGE_BUFFERS;
        }
    }

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
static void srwirpcq_free_message (api_receive_context receive_context_in, api_message_buffer *const buffer)
{
    srwirpcq_receiver_context *const receive_context = (srwirpcq_receiver_context *) receive_context_in;
    srwirpcq_message_buffer_receive_context *const buffer_context = &receive_context->buffer_contexts[buffer->buffer_index];
    struct ibv_recv_wr *bad_recv_wr = NULL;
    struct ibv_send_wr *bad_send_wr = NULL;
    int send_flags;
    int rc;

    CHECK_ASSERT (buffer_context->owned_by_application);

    /* Post the work request to receive the next available message for this buffer */
    rc = ibv_post_recv (receive_context->receiver_qp, &buffer_context->message_available_wr, &bad_recv_wr);
    CHECK_ASSERT (rc == 0);

    /* Wait for the freed buffer send work request completion, which is signalled on one message in every NUM_MESSAGE_BUFFERS */
    if (receive_context->freed_buffer_cq_pacing == NUM_MESSAGE_BUFFERS)
    {
        int num_completions;
        struct ibv_wc wc;

        do
        {
            num_completions = ibv_poll_cq (receive_context->freed_buffer_cq, 1, &wc);
        } while (num_completions == 0);
        CHECK_ASSERT ((num_completions == 1) && (wc.status == IBV_WC_SUCCESS));
        receive_context->freed_buffer_cq_pacing = 0;
    }

    /* Send the index of the freed buffer to the sender */
    send_flags = 0;
    if (receive_context->freed_buffer_cq_pacing == 0)
    {
        send_flags |= IBV_SEND_SIGNALED;
    }
    buffer_context->freed_buffer_wr.send_flags = send_flags;
    receive_context->freed_buffer_cq_pacing++;
    rc = ibv_post_send (receive_context->receiver_qp, &buffer_context->freed_buffer_wr, &bad_send_wr);
    CHECK_ASSERT (rc == 0);

    buffer_context->owned_by_application = false;
    buffer_context->message_available = false;
}

/**
 * @brief Set the function pointers to transfer messages with the sender using RDMA write with immediate and the receive polling the CQ
 * @param[out] functions Contains the functions pointers to the message transfer functions in this source file
 */
void sender_rdma_write_with_imm_receiver_poll_cq_set_functions (message_communication_functions *const functions)
{
    functions->description = "sender using RDMA write with immediate and the receiver polling the CQ";
    functions->initialise = srwirpcq_initialise;
    functions->finalise = srwirpcq_finalise;
    functions->get_send_buffer = srwirpcq_get_send_buffer;
    functions->send_message = srwirpcq_send_message;
    functions->await_message = srwirpcq_await_message;
    functions->free_message = srwirpcq_free_message;
}
