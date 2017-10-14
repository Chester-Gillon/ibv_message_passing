/*
 * @file ibv_message_bw_receive.c
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains functions to receive messages on a communication path for the ibv_message_bw program
 */

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"

/** Contains the context used to receive messages from one communication path over a pair of connected Infiniband ports */
typedef struct rx_message_context_s
{
    /** The definition of the communication path for which this context receives messages on */
    communication_path_definition path_def;
    /** The Infiniband port this context receives messages on */
    ib_port_endpoint endpoint;
    /** Used to exchange Queue Pair information with the transmit end of the communication path */
    communication_path_slp_connection slp_connection;
    /** The buffer in the receiver which is used to:
     *  - Receive messages from the transmitter
     *  - Transmit freed message sequence numbers to the transmitter */
    memory_buffer receive_buffer;
    /** The memory region for receive_buffer */
    struct ibv_mr *receive_mr;
    /** Completion queue for sending the freed sequence number as part of flow control */
    struct ibv_cq *freed_sequence_number_cq;
    /** Queue Pair for sending the freed sequence number */
    struct ibv_qp *freed_sequence_number_qp;
    /** The Packet Sequence Number for freed_sequence_number_qp */
    uint32_t freed_sequence_number_psn;
    /** The maximum number of bytes which can be sent inline on freed_sequence_number_qp */
    uint32_t freed_sequence_number_qp_max_inline_data;
} rx_message_context;

/**
 * @details Perform the initialisation of the context for the reception of messages on a communication path
 *          which creates local resources without requiring the identity of the Queue Pair on the source.
 *
 *          This publishes the Queue Pair of the receive endpoint using SLP, so that the transmit end point can connect.
 * @param[in] path_def Defines the communication path to create the receive end for
 * @return Returns a pointer to the context to receive messages on
 */
rx_message_context_handle message_receive_create_local (const communication_path_definition *const path_def)
{
    rx_message_context_handle context = cache_line_aligned_calloc (1, sizeof (rx_message_context));
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;
    const bool is_tx_end = false;

    /* Open the Infiniband device port */
    context->path_def = *path_def;
    open_ib_port_endpoint (&context->endpoint, &context->path_def);

    /* Create the and register receive memory buffer */
    create_memory_buffer (&context->receive_buffer, is_tx_end, &context->path_def);
    context->receive_mr = ibv_reg_mr (context->endpoint.device_pd, context->receive_buffer.buffer,
            context->receive_buffer.size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (context->receive_mr == NULL)
    {
        perror ("ibv_reg_mr for receive_mr failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the receiver.
     * The rationale for the queue sizes are that only one in every NUM_MESSAGE_BUFFERS is signalled for send completion so:
     * - The completion queue only need one entry.
     * - The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *   as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    context->freed_sequence_number_cq = ibv_create_cq (context->endpoint.device_context, 1, NULL, NULL, 0);
    if (context->freed_sequence_number_cq == NULL)
    {
        perror ("ibv_create_cq freed_sequence_number_cq failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = context->freed_sequence_number_cq;
    qp_init_attr.recv_cq = context->freed_sequence_number_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 2 * context->path_def.num_message_buffers;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = false;
    context->freed_sequence_number_qp = ibv_create_qp (context->endpoint.device_pd, &qp_init_attr);
    if (context->freed_sequence_number_qp == NULL)
    {
        perror ("ibv_create_qp freed_sequence_number_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, context->freed_sequence_number_qp, "freed_sequence_number_qp");
    context->freed_sequence_number_psn = get_random_psn ();
    context->freed_sequence_number_qp_max_inline_data = get_max_inline_data (context->freed_sequence_number_qp);

    /* Transition the receiver Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = context->path_def.port_num;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (context->freed_sequence_number_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    if (rc != 0)
    {
        perror ("ibv_modify_qp freed_sequence_number_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_INIT, context->freed_sequence_number_qp, "freed_sequence_number_qp");

    /* Publish the receive buffer using SLP */
    intialise_slp_connection (&context->slp_connection, is_tx_end, &context->path_def);
    register_memory_buffer_with_slp (&context->slp_connection, &context->endpoint, context->freed_sequence_number_psn,
            context->receive_mr, context->freed_sequence_number_qp);

    return context;
}

/**
 * @details Complete the initialisation of the context for the reception of messages on a communication path by:
 *          - Transition the receive Queue Pair, used to send freed sequence numbers, to the Ready To Send State
 *          - Initialise the free sequence number transmit buffers
 *
 *          This obtains the Queue Pair and memory region for the transmit context from SLP
 * @param[in,out] context The receive message context to complete the initialisation for
 */
void message_receive_attach_remote (rx_message_context_handle context)
{
    /* Wait for the attributes of the remote endpoint to be retrieved from SLP */
    get_remote_memory_buffer_from_slp (&context->slp_connection);
}

/**
 * @brief Free the resources used by the context for the reception of messages on a communication path
 * @param[in,out] context The receive message context to free the resources for
 */
void message_receive_finalise (rx_message_context_handle context)
{
    int rc;

    close_slp_connection (&context->slp_connection);

    /* Destroy the queues */
    rc = ibv_destroy_qp (context->freed_sequence_number_qp);
    if (rc != 0)
    {
        perror ("ibv_destroy_qp freed_sequence_number_qp failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (context->freed_sequence_number_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cq freed_sequence_number_cq failed");
        exit (EXIT_FAILURE);
    }

    /* De-register and free the receive buffers */
    rc = ibv_dereg_mr (context->receive_mr);
    check_assert (rc == 0, "ibv_dereg_mr");
    release_memory_buffer (&context->receive_buffer);

    /* Close the Infiniband port */
    close_ib_port_endpoint (&context->endpoint);
    free (context);
}
