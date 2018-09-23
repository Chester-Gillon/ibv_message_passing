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

/** Defines the internal information for one message buffer used to receive the message and transmit flow control */
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
     *  - message_available_sequence_number : A message is available
     *
     *  Any other value indicates an error */
    uint32_t message_not_available_sequence_number;
    uint32_t message_available_sequence_number;
    /** Used to check usage of the message buffer:
     *  - If false in use by the receive API functions.
     *  - If true in use by the application for processing the received message contents */
    bool owned_by_application;
    /** Points at the receive buffer for the location of the freed sequence number */
    uint32_t *freed_sequence_number;
} rx_message_buffer;

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
    /** Array of length path_def.num_message_buffers which point at each message in receive_buffer */
    rx_api_message_buffer *api_message_buffers;
    /** Array of length path_def.num_message_buffers which contains the internal information used to receive messages */
    rx_message_buffer *rx_message_buffers;
    /** The circular buffer index for the next message message to check for message receipt */
    uint32_t next_receive_buffer_index;
    /** Used to control only signalling freed sequence number send completion on one in path_def.num_message_buffers messages */
    uint32_t freed_sequence_number_cq_pacing;
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
    open_ib_port_endpoint (&context->endpoint, &context->path_def, is_tx_end);

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
     * The rationale for the queue sizes are that only one in every path_def.num_message_buffers is signalled for send completion so:
     * a) The completion queue only need one entry in normal operation, but is set to have one entry for each WQE to prevent
     *    the completion queue from over-running in the event that the work-requests fail with an error
     *    (e.g. when the transmitting process exits abnormally).
     * b) The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *    as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    context->freed_sequence_number_cq = ibv_create_cq (context->endpoint.device_context, context->path_def.num_message_buffers,
            NULL, NULL, 0);
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
    qp_attr.port_num = context->path_def.destination_port_num;
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
 * @details Perform the initialisation of the context for the receiver of test messages for each message buffer
 *          which may be received.
 *          This sets the Infiniband structures for the freed sequence number send operations using the fixed buffer attributes.
 * @param[in,out] context The receive message context to complete the initialisation for
 */
static void initialise_receive_message_buffers (rx_message_context_handle context)
{
    uint32_t buffer_index;
    uint64_t buffer_offset;
    uint64_t header_offset;
    uint64_t data_offset;
    uint64_t freed_sequence_number_offset;

    context->api_message_buffers = cache_line_aligned_calloc (context->path_def.num_message_buffers, sizeof (rx_api_message_buffer));
    context->rx_message_buffers = cache_line_aligned_calloc (context->path_def.num_message_buffers, sizeof (rx_message_buffer));
    buffer_offset = 0;
    for (buffer_index = 0; buffer_index < context->path_def.num_message_buffers; buffer_index++)
    {
        rx_api_message_buffer *const api_buffer = &context->api_message_buffers[buffer_index];
        rx_message_buffer *const rx_buffer = &context->rx_message_buffers[buffer_index];

        /* Set the header, data and freed sequence number offsets for this message */
        api_buffer->context = context;
        api_buffer->buffer_index = buffer_index;
        header_offset = buffer_offset;
        buffer_offset += align_to_cache_line_size (sizeof (message_header));
        api_buffer->header = (message_header *) &context->receive_buffer.buffer[header_offset];
        data_offset = buffer_offset;
        buffer_offset += align_to_cache_line_size (context->path_def.max_message_size);
        api_buffer->data = &context->receive_buffer.buffer[data_offset];
        freed_sequence_number_offset = buffer_offset;
        buffer_offset += align_to_cache_line_size (sizeof (uint32_t));
        rx_buffer->freed_sequence_number = (uint32_t *) &context->receive_buffer.buffer[freed_sequence_number_offset];

        /* Set the scatter-gather entry to transmit the freed message sequence number */
        rx_buffer->freed_message_sge.lkey = context->receive_mr->lkey;
        rx_buffer->freed_message_sge.addr = (uintptr_t) rx_buffer->freed_sequence_number;
        rx_buffer->freed_message_sge.length = sizeof (uint32_t);

        /* Set the work request to transmit the freed message sequence number */
        rx_buffer->freed_message_wr.wr_id = buffer_index;
        rx_buffer->freed_message_wr.sg_list = &rx_buffer->freed_message_sge;
        rx_buffer->freed_message_wr.num_sge = 1;
        rx_buffer->freed_message_wr.next = NULL;
        rx_buffer->freed_message_wr.opcode = IBV_WR_RDMA_WRITE;
        rx_buffer->freed_message_wr.send_flags = 0;
        if (sizeof (uint32_t) <= context->freed_sequence_number_qp_max_inline_data)
        {
            rx_buffer->freed_message_wr.send_flags |= IBV_SEND_INLINE;
        }
        rx_buffer->freed_message_wr.wr.rdma.rkey = context->slp_connection.remote_attributes.rkey;
        rx_buffer->freed_message_wr.wr.rdma.remote_addr =
                context->slp_connection.remote_attributes.addr + freed_sequence_number_offset;

        /* Initialise the sequence number management */
        rx_buffer->owned_by_application = false;
        rx_buffer->message_not_available_sequence_number = 0;
        rx_buffer->message_available_sequence_number = buffer_index + 1;
    }

    context->next_receive_buffer_index = 0;
    context->freed_sequence_number_cq_pacing = 0;
}

/**
 * @detail Perform the first part of the handshake of connection of a receive communication path by waiting for the
 *         attributes of the remote endpoint, and then transition the Queue-Pair of this endpoint to the ready-to-receive state.
 * @param[in,out] context The receive message context to complete the initialisation for
 */
void message_receive_attach_remote_pre_rtr (rx_message_context_handle context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    /* Wait for the attributes of the remote endpoint to be retrieved from SLP */
    get_remote_memory_buffer_from_slp (&context->slp_connection);

    /* Transition the receiver Queue Pair to the Ready to Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = context->endpoint.port_attributes.active_mtu;
    qp_attr.dest_qp_num = context->slp_connection.remote_attributes.qp_num;
    qp_attr.rq_psn = context->slp_connection.remote_attributes.psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 12; /* 0.64 milliseconds delay */
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = context->slp_connection.remote_attributes.lid;
    qp_attr.ah_attr.sl = DEFAULT_SERVICE_LEVEL;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = context->path_def.destination_port_num;
    rc = ibv_modify_qp (context->freed_sequence_number_qp, &qp_attr,
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
    verify_qp_state (IBV_QPS_RTR, context->freed_sequence_number_qp, "freed_sequence_number_qp");

    /* Report that this endpoint is ready-to-receive */
    report_local_memory_buffer_rtr_with_slp (&context->slp_connection);
}

/**
 * @detail Perform the second part of the handshake of connection of a receive communication path by waiting for the
 *         remote endpoint to be ready-to-receive, and then transition the Queue-Pair of this endpoint to the ready-to-send state.
 * @param[in,out] context The receive message context to complete the initialisation for
 */
void message_receive_attach_remote_post_rtr (rx_message_context_handle context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    await_remote_memory_buffer_rtr_from_slp (&context->slp_connection);

    /* Transition the sender Queue Pair to the Ready to Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = context->freed_sequence_number_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7;
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (context->freed_sequence_number_qp, &qp_attr,
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
    verify_qp_state (IBV_QPS_RTS, context->freed_sequence_number_qp, "freed_sequence_number_qp");

    initialise_receive_message_buffers (context);
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
    free (context->api_message_buffers);
    free (context->rx_message_buffers);
    release_memory_buffer (&context->receive_buffer);

    /* Close the Infiniband port */
    close_ib_port_endpoint (&context->endpoint);
    free (context);
}

/**
 * @brief Poll for receipt of a message
 * @details The contents of the received message remains valid until the returned buffer is freed by a
 *          call to free_message(). Flow control prevents a received message from being overwritten until freed
 * @param[in,out] context The receive context to receive the message on
 * @return Returns a pointer to the received message buffer, or NULL if no message is currently available
 */
rx_api_message_buffer *poll_rx_message (rx_message_context_handle context)
{
    const uint32_t buffer_index = context->next_receive_buffer_index;
    rx_api_message_buffer *api_buffer = &context->api_message_buffers[buffer_index];
    rx_message_buffer *const rx_buffer = &context->rx_message_buffers[buffer_index];
    const uint32_t *const receive_sequence_number = &api_buffer->header->sequence_number;
    bool message_available = false;
    const uint32_t sampled_sequence_number = __atomic_load_n (receive_sequence_number, __ATOMIC_ACQUIRE);

    if (sampled_sequence_number == rx_buffer->message_available_sequence_number)
    {
        message_available = true;
    }
    else
    {
        CHECK_ASSERT (sampled_sequence_number == rx_buffer->message_not_available_sequence_number);
    }

    if (message_available)
    {
        /* Advance to the next expected receive buffer */
        context->next_receive_buffer_index = (context->next_receive_buffer_index + 1) % context->path_def.num_message_buffers;

        /* Return the received message */
        rx_buffer->owned_by_application = true;
    }
    else
    {
        api_buffer = NULL;
    }

    return api_buffer;
}

/**
 * @brief Wait for a message to be received, using a busy-poll
 * @details The contents of the received message remains valid until the returned buffer is freed by a
 *          call to free_message(). Flow control prevents a received message from being overwritten until freed
 * @param[in,out] context The receive context to receive the message on
 * @return Returns a pointer to the received message buffer
 */
rx_api_message_buffer *await_message (rx_message_context_handle context)
{
    rx_api_message_buffer *api_buffer;

    do
    {
        api_buffer = poll_rx_message (context);
    } while (api_buffer == NULL);

    return api_buffer;
}

/**
 * @brief Mark a receive message buffer after the received message has been freed
 * @details This implements flow control by indicating to sender that the buffer is now free for another message
 * @param[in,out] api_buffer The received message buffer to free
 */
void free_message (rx_api_message_buffer *const api_buffer)
{
    rx_message_buffer *const rx_buffer = &api_buffer->context->rx_message_buffers[api_buffer->buffer_index];
    struct ibv_send_wr *bad_wr = NULL;
    int rc;

    CHECK_ASSERT (rx_buffer->owned_by_application);

    /* Wait for the freed buffer send work request completion, which is signalled on one message in every num_message_buffers */
    if (api_buffer->context->freed_sequence_number_cq_pacing == api_buffer->context->path_def.num_message_buffers)
    {
        int num_completions;
        struct ibv_wc wc;

        do
        {
            num_completions = ibv_poll_cq (api_buffer->context->freed_sequence_number_cq, 1, &wc);
        } while (num_completions == 0);
        CHECK_ASSERT ((num_completions == 1) && (wc.status == IBV_WC_SUCCESS));
        api_buffer->context->freed_sequence_number_cq_pacing = 0;
    }

    /* Transmit the freed sequence number to the sender, to indicate the buffer can be reused */
    __atomic_store_n (rx_buffer->freed_sequence_number, rx_buffer->message_available_sequence_number, __ATOMIC_RELEASE);
    if (api_buffer->context->freed_sequence_number_cq_pacing == 0)
    {
        rx_buffer->freed_message_wr.send_flags |= IBV_SEND_SIGNALED;
    }
    else
    {
        rx_buffer->freed_message_wr.send_flags &= ~IBV_SEND_SIGNALED;
    }
    api_buffer->context->freed_sequence_number_cq_pacing++;
    rc = ibv_post_send (api_buffer->context->freed_sequence_number_qp, &rx_buffer->freed_message_wr, &bad_wr);
    CHECK_ASSERT (rc == 0);

    /* Advance to the next expected sequence number */
    rx_buffer->message_not_available_sequence_number = rx_buffer->message_available_sequence_number;
    rx_buffer->message_available_sequence_number += api_buffer->context->path_def.num_message_buffers;
    rx_buffer->owned_by_application = false;
}
