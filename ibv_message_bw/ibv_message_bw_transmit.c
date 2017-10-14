/*
 * @file ibv_message_bw_transmit.c
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains functions to transmit messages on a communication path for the ibv_message_bw program
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

/** To enforce the header being received after the data of each message is available,
 *  the data is sent in one WQE followed by the header */
#define MESSAGE_DATA_WQE_INDEX   0
#define MESSAGE_HEADER_WQE_INDEX 1
#define NUM_WQES_PER_MESSAGE     2

/** Contains the context used to transmit messages on one communication path over a pair of connected Infiniband ports */
typedef struct tx_message_context_s
{
    /** The definition of the communication path for which this context transmits messages on */
    communication_path_definition path_def;
    /** The Infiniband port this context transmits messages on */
    ib_port_endpoint endpoint;
    /** Used to exchange Queue Pair information with the receive end of the communication path */
    communication_path_slp_connection slp_connection;
    /** The buffer in the transmitter which is used to:
     *  - Transmit messages to the receiver
     *  - Receive freed message sequence numbers from the receiver */
    memory_buffer transmit_buffer;
    /** The memory region for transmit_buffer */
    struct ibv_mr *transmit_mr;
    /** Completion queue for transmitting the messages */
    struct ibv_cq *message_transmit_cq;
    /** Queue Pair for transmitting the messages */
    struct ibv_qp *message_transmit_qp;
    /** The Packet Sequence Number for message_transmit_qp */
    uint32_t message_transmit_psn;
    /** The maximum number of bytes which can be sent inline on transmit_send_qp */
    uint32_t message_transmit_qp_max_inline_data;
} tx_message_context;

/**
 * @details Perform the initialisation of the context for the transmission of messages on a communication path
 *          which creates local resources without requiring the identity of the Queue Pair on the destination.
 *
 *          This publishes the Queue Pair of the transmit endpoint using SLP, so that the receive endpoint can connect.
 * @param[in] path_def Defines the communication path to create the transmit end for
 * @return Returns a pointer to the context to transmit messages on
 */
tx_message_context_handle message_transmit_create_local (const communication_path_definition *const path_def)
{
    tx_message_context_handle context = cache_line_aligned_calloc (1, sizeof (tx_message_context));
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;
    const bool is_tx_end = true;

    /* Open the Infiniband device port */
    context->path_def = *path_def;
    open_ib_port_endpoint (&context->endpoint, &context->path_def);

    /* Create and register the transmit memory buffer */
    create_memory_buffer (&context->transmit_buffer, is_tx_end, &context->path_def);
    context->transmit_mr = ibv_reg_mr (context->endpoint.device_pd, context->transmit_buffer.buffer,
            context->transmit_buffer.size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    if (context->transmit_mr == NULL)
    {
        perror ("ibv_reg_mr for transmit_mr failed");
        exit (EXIT_FAILURE);
    }

    /* Create the queues used by the transmitter.
     * The rationale for the queue sizes are that only one in every path_def.num_message_buffers is signalled for send completion so:
     * - The completion queue only need one entry.
     * - The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *   as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    context->message_transmit_cq = ibv_create_cq (context->endpoint.device_context, 1, NULL, NULL, 0);
    if (context->message_transmit_cq == NULL)
    {
        perror ("ibv_create_cq message_transmit_cq failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = context->message_transmit_cq;
    qp_init_attr.recv_cq = context->message_transmit_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 2 * context->path_def.num_message_buffers * NUM_WQES_PER_MESSAGE;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = false;
    context->message_transmit_qp = ibv_create_qp (context->endpoint.device_pd, &qp_init_attr);
    if (context->message_transmit_qp == NULL)
    {
        perror ("ibv_create_qp message_transmit_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_RESET, context->message_transmit_qp, "message_transmit_qp");
    context->message_transmit_psn = get_random_psn ();
    context->message_transmit_qp_max_inline_data = get_max_inline_data (context->message_transmit_qp);

    /* Transition the transmit Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = context->path_def.port_num;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (context->message_transmit_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    if (rc != 0)
    {
        perror ("ibv_modify_qp message_transmit_qp failed");
        exit (EXIT_FAILURE);
    }
    verify_qp_state (IBV_QPS_INIT, context->message_transmit_qp, "message_transmit_qp");

    /* Publish the transmit buffer using SLP */
    intialise_slp_connection (&context->slp_connection, is_tx_end, &context->path_def);
    register_memory_buffer_with_slp (&context->slp_connection, &context->endpoint, context->message_transmit_psn,
            context->transmit_mr, context->message_transmit_qp);

    return context;
}

/**
 * @details Complete the initialisation of the context for the transmission of messages on a communication path by:
 *          - Transition the transmit Queue Pair, used to send messages, to the Ready To Send State
 *          - Initialise the message transmit buffers
 *
 *          This obtains the Queue Pair and memory region for the receive context from SLP
 * @param[in,out] context The transmit message context to complete the initialisation for
 */
void message_transmit_attach_remote (tx_message_context_handle context)
{
    /* Wait for the attributes of the remote endpoint to be retrieved from SLP */
    get_remote_memory_buffer_from_slp (&context->slp_connection);
}

/**
 * @brief Free the resources used by the context for the transmission of messages on a communication path
 * @param[in,out] context The transmit message context to free the resources for
 */
void message_transmit_finalise (tx_message_context_handle context)
{
    int rc;

    close_slp_connection (&context->slp_connection);

    /* Destroy the queues */
    rc = ibv_destroy_qp (context->message_transmit_qp);
    if (rc != 0)
    {
        perror ("ibv_destroy_qp message_transmit_qp failed");
        exit (EXIT_FAILURE);
    }
    rc = ibv_destroy_cq (context->message_transmit_cq);
    if (rc != 0)
    {
        perror ("ibv_destroy_cp message_transmit_cq failed");
        exit (EXIT_FAILURE);
    }

    /* De-register and free the transmit buffers */
    rc = ibv_dereg_mr (context->transmit_mr);
    check_assert (rc == 0, "ibv_dereg_mr");
    release_memory_buffer (&context->transmit_buffer);

    /* Close the Infiniband port */
    close_ib_port_endpoint (&context->endpoint);
    free (context);
}
