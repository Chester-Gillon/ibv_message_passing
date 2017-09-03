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

#include "ibv_functional_loopback_test_interface.h"

/** To enforce the header being received after the data of each message is received,
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
} srwrp_receiver_context;

static void srwrp_initialise (srwrp_sender_context *const send_context, srwrp_receiver_context *const receive_context)
{
    struct ibv_qp_init_attr qp_attr;

    /* Create and register the memory for the send/receive buffers */
    send_context->send_buffer = page_aligned_alloc (sizeof (srwrp_sender_buffer));
    receive_context->receive_buffer = page_aligned_alloc (sizeof (srwrp_receiver_buffer));

    send_context->send_mr = ibv_reg_mr (ibv_loopback_device_pd, send_context->send_buffer, sizeof (srwrp_sender_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (send_context->send_mr == NULL)
    {
        perror ("ibv_reg_mrv for send failed");
        exit (EXIT_FAILURE);
    }

    receive_context->receive_mr = ibv_reg_mr (ibv_loopback_device_pd, receive_context->receive_buffer, sizeof (srwrp_receiver_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (receive_context->receive_mr == NULL)
    {
        perror ("ibv_reg_mrv for receive failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the sender */
    memset (&qp_attr, 0, sizeof (qp_attr));
    send_context->message_send_cq = ibv_create_cq (ibv_loopback_device, TOTAL_MESSAGE_SEND_QUEUE_SIZE, NULL, NULL, 0);
    if (send_context->message_send_cq == NULL)
    {
        perror ("ibv_create_cq send failed");
        exit (EXIT_FAILURE);
    }

    qp_attr.qp_context = NULL;
    qp_attr.send_cq = send_context->message_send_cq;
    qp_attr.recv_cq = send_context->message_send_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_attr.srq = NULL;
    qp_attr.cap.max_send_wr = TOTAL_MESSAGE_SEND_QUEUE_SIZE;
    qp_attr.cap.max_send_sge = 1;
    qp_attr.cap.max_recv_wr = 0;
    qp_attr.cap.max_recv_sge = 0;
    qp_attr.cap.max_inline_data = 0;
    qp_attr.qp_type = IBV_QPT_RC;
    qp_attr.sq_sig_all = true;
    send_context->message_send_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_attr);
    if (send_context->message_send_qp == NULL)
    {
        perror ("ibv_create_qp send failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the receiver */
    memset (&qp_attr, 0, sizeof (qp_attr));
    receive_context->freed_sequence_number_cq = ibv_create_cq (ibv_loopback_device, NUM_MESSAGE_BUFFERS, NULL, NULL, 0);
    if (receive_context->freed_sequence_number_cq == NULL)
    {
        perror ("ibv_create_cq receive failed");
        exit (EXIT_FAILURE);
    }

    qp_attr.qp_context = NULL;
    qp_attr.send_cq = receive_context->freed_sequence_number_cq;
    qp_attr.recv_cq = receive_context->freed_sequence_number_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_attr.srq = NULL;
    qp_attr.cap.max_send_wr = NUM_MESSAGE_BUFFERS;
    qp_attr.cap.max_send_sge = 1;
    qp_attr.cap.max_recv_wr = 0;
    qp_attr.cap.max_recv_sge = 0;
    qp_attr.cap.max_inline_data = 0;
    qp_attr.qp_type = IBV_QPT_RC;
    qp_attr.sq_sig_all = true;
    receive_context->freed_sequence_number_qp = ibv_create_qp (ibv_loopback_device_pd, &qp_attr);
    if (receive_context->freed_sequence_number_qp == NULL)
    {
        perror ("ibv_create_qp receive failed");
        exit (EXIT_FAILURE);
    }
}

static void srwrp_finalise (srwrp_sender_context *const send_context, srwrp_receiver_context *const receive_context)
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

    /* De-register and free the send/receive buffers */
    rc = ibv_dereg_mr (send_context->send_mr);
    if (rc != 0)
    {
        perror ("ibv_dereg_mr send failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_dereg_mr (receive_context->receive_mr);
    if (rc != 0)
    {
        perror ("ibv_dereg_mr receive failed");
        exit (EXIT_FAILURE);
    }
    free (send_context->send_buffer);
    free (receive_context->receive_buffer);
}

void test_sender_rdma_write_receiver_passive (void)
{
    srwrp_sender_context send_context;
    srwrp_receiver_context receive_context;

    memset (&send_context, 0, sizeof (send_context));
    memset (&receive_context, 0, sizeof (receive_context));

    srwrp_initialise (&send_context, &receive_context);
    srwrp_finalise (&send_context, &receive_context);
}
