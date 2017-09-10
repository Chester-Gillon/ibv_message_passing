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
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
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

/** This structure defines the buffer used by the sender to transmit messages, and receive flow control.
 *  Where this buffer is accessible by the Infiniband device. */
typedef struct
{
    /** The messages which are transmitted */
    test_message transmit_messages[NUM_MESSAGE_BUFFERS];
    /** Used to receive the sequence numbers of the freed messages, to provide flow control */
    uint32_t freed_sequence_numbers[NUM_MESSAGE_BUFFERS];
} srwrp_sender_buffer;

/** This structure defines the buffer used by the receiver to receive messages, and transmit flow control.
 *  Where this buffer is accessible by the Infiniband device. */
typedef struct
{
    /** The messages which are received */
    test_message receive_messages[NUM_MESSAGE_BUFFERS];
    /** Used to transmit the sequence numbers of the freed messages, to provide flow control */
    uint32_t freed_sequence_numbers[NUM_MESSAGE_BUFFERS];
} srwrp_receiver_buffer;

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
} srwrp_sender_context;

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

    /* Create the queues used by the sender */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    send_context->message_send_cq = ibv_create_cq (ibv_loopback_device, TOTAL_MESSAGE_SEND_QUEUE_SIZE, NULL, NULL, 0);
    if (send_context->message_send_cq == NULL)
    {
        perror ("ibv_create_cq send failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = send_context->message_send_cq;
    qp_init_attr.recv_cq = send_context->message_send_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = TOTAL_MESSAGE_SEND_QUEUE_SIZE;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = true;
    send_context->message_send_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_init_attr);
    if (send_context->message_send_qp == NULL)
    {
        perror ("ibv_create_qp send failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, send_context->message_send_qp, "message_send_qp");
    send_context->message_send_psn = get_random_psn ();

    /* Transition the sender Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = SOURCE_PORT_NUM;
    qp_attr.qp_access_flags = 0;
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

    /* Create and register the memory for the send buffers */
    receive_context->receive_buffer = page_aligned_calloc (1, sizeof (srwrp_receiver_buffer));

    receive_context->receive_mr = ibv_reg_mr (ibv_loopback_device_pd, receive_context->receive_buffer, sizeof (srwrp_receiver_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (receive_context->receive_mr == NULL)
    {
        perror ("ibv_reg_mrv for receive failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the receiver */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    receive_context->freed_sequence_number_cq = ibv_create_cq (ibv_loopback_device, NUM_MESSAGE_BUFFERS, NULL, NULL, 0);
    if (receive_context->freed_sequence_number_cq == NULL)
    {
        perror ("ibv_create_cq receive failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = receive_context->freed_sequence_number_cq;
    qp_init_attr.recv_cq = receive_context->freed_sequence_number_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = NUM_MESSAGE_BUFFERS;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = true;
    receive_context->freed_sequence_number_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_init_attr);
    if (receive_context->freed_sequence_number_qp == NULL)
    {
        perror ("ibv_create_qp receive failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, receive_context->freed_sequence_number_qp, "freed_sequence_number_qp");
    receive_context->freed_sequence_number_psn = get_random_psn ();

    /* Transition the receiver Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = DESTINATION_PORT_NUM;
    qp_attr.qp_access_flags = 0;
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
 * @details Complete the initialisation of the context for the sender of the test messages by:
 *          - Transition the sender Queue Pair, used to send messages, to the Ready To Send State
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
    qp_attr.max_dest_rd_atomic = 1;
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
    qp_attr.rq_psn = send_context->message_send_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7;
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 1;
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
}

/**
 * @details Complete the initialisation of the context for the receiver of the test messages by:
 *          - Transition the receiver Queue Pair, used to report freed message buffers, to the Ready To Send State
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
    qp_attr.max_dest_rd_atomic = 1;
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
    qp_attr.rq_psn = receive_context->freed_sequence_number_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7;
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 1;
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

void test_sender_rdma_write_receiver_passive (void)
{
    srwrp_sender_context send_context;
    srwrp_receiver_context receive_context;

    srwrp_sender_create_local (&send_context);
    srwrp_receiver_create_local (&receive_context);
    srwrp_sender_attach_remote (&send_context, &receive_context);
    srwrp_receiver_attach_remote (&receive_context, &send_context);
    srwrp_sender_finalise (&send_context);
    srwrp_receiver_finalise (&receive_context);
}
