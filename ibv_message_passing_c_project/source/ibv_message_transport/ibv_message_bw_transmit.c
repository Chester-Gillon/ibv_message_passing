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

/** Defines the internal information for one message buffer used to transmit the message and receive flow control */
typedef struct
{
    /** Contains the scatter-gather entries to transfer the message data then header.
     *  Set at initialisation, and only the size for the message data has to be changed as messages are sent */
    struct ibv_sge transmit_sges[NUM_WQES_PER_MESSAGE];
    /** Contains the work requests to transfer the message data then header.
     *  Set at initialisation, and do not require modification as messages are sent */
    struct ibv_send_wr transmit_wrs[NUM_WQES_PER_MESSAGE];
    /** The sequence number for the next message to be sent from this buffer */
    uint32_t next_transmit_sequence_number;
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
    /** Points at the transmit buffer for the location of the freed sequence number */
    uint32_t *freed_sequence_number;
} tx_message_buffer;

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
    /** Array of length path_def.num_message_buffers which point at each message in transmit_buffer */
    tx_api_message_buffer *api_message_buffers;
    /** Array of length path_def.num_message_buffers which contains the internal information used to transmit messages */
    tx_message_buffer *tx_message_buffers;
    /** The circular buffer index for the next message buffer to pass to the application for populating */
    uint32_t next_transmit_buffer_index;
    /** Used to control only signalling message send completion on one in every path_def.num_message_buffers messages */
    uint32_t message_transmit_cq_pacing;
    /** The number of outstanding completion queue entries which have not been consumed */
    uint32_t num_outstanding_completions;
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
    const uint32_t max_queued_wqes = path_def->num_message_buffers * NUM_WQES_PER_MESSAGE;
    tx_message_context_handle context = cache_line_aligned_calloc (1, sizeof (tx_message_context));
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;
    const bool is_tx_end = true;

    /* Open the Infiniband device port */
    context->path_def = *path_def;
    open_ib_port_endpoint (&context->endpoint, &context->path_def, is_tx_end);

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
     * a) The completion queue only need one entry in normal operation, but is set to have one entry for each WQE to prevent
     *    the completion queue from over-running in the event that the work-requests fail with an error
     *    (e.g. when the receiving process exits abnormally).
     * b) The Queue Pair requires twice the number of work-requests than buffers to prevent the work queue from filling up
     *    as far as ibv_post_send() is concerned. */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    context->message_transmit_cq = ibv_create_cq (context->endpoint.device_context, max_queued_wqes, NULL, NULL, 0);
    if (context->message_transmit_cq == NULL)
    {
        perror ("ibv_create_cq message_transmit_cq failed");
        exit (EXIT_FAILURE);
    }

    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = context->message_transmit_cq;
    qp_init_attr.recv_cq = context->message_transmit_cq; /* Not used, but need to prevent ibv_create_qp failing */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 2 * max_queued_wqes;
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
    verify_qp_state (IBV_QPS_RESET, context->message_transmit_qp, "message_transmit_qp", NULL);
    context->message_transmit_psn = get_random_psn ();
    context->message_transmit_qp_max_inline_data = get_max_inline_data (context->message_transmit_qp);

    /* Transition the transmit Queue Pair to the Init state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = context->path_def.source_port_num;
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
    verify_qp_state (IBV_QPS_INIT, context->message_transmit_qp, "message_transmit_qp", NULL);

    /* Publish the transmit buffer using SLP */
    intialise_slp_connection (&context->slp_connection, is_tx_end, &context->path_def);
    register_memory_buffer_with_slp (&context->slp_connection, &context->endpoint, context->message_transmit_psn,
            context->transmit_mr, context->message_transmit_qp);

    return context;
}

/**
 * @details Perform the initialisation of the context for the transmitter of test messages for each message buffer
 *          which may be sent.
 *          This sets the Infiniband structures for the send operations using the fixed buffer attributes,
 *          so that only the data size needs to be adjusted as messages are sent.
 * @param[in,out] context The transmit message context to complete the initialisation for
 */
static void initialise_transmit_message_buffers (tx_message_context_handle context)
{
    uint32_t buffer_index;
    uint64_t buffer_offset;
    uint64_t header_offset;
    uint64_t data_offset;
    uint64_t freed_sequence_number_offset;

    context->api_message_buffers =
            cache_line_aligned_calloc (context->path_def.num_message_buffers, sizeof (tx_api_message_buffer));
    context->tx_message_buffers = cache_line_aligned_calloc (context->path_def.num_message_buffers, sizeof (tx_message_buffer));
    buffer_offset = 0;
    for (buffer_index = 0; buffer_index < context->path_def.num_message_buffers; buffer_index++)
    {
        tx_api_message_buffer *const api_buffer = &context->api_message_buffers[buffer_index];
        tx_message_buffer *const tx_buffer = &context->tx_message_buffers[buffer_index];
        struct ibv_sge *const data_sge = &tx_buffer->transmit_sges[MESSAGE_DATA_WQE_INDEX];
        struct ibv_sge *const header_sge = &tx_buffer->transmit_sges[MESSAGE_HEADER_WQE_INDEX];
        struct ibv_send_wr *const data_wr = &tx_buffer->transmit_wrs[MESSAGE_DATA_WQE_INDEX];
        struct ibv_send_wr *const header_wr = &tx_buffer->transmit_wrs[MESSAGE_HEADER_WQE_INDEX];

        /* Set the header, data and freed sequence number offsets for this message */
        api_buffer->context = context;
        api_buffer->buffer_index = buffer_index;
        header_offset = buffer_offset;
        buffer_offset += align_to_cache_line_size (sizeof (message_header));
        api_buffer->header = (message_header *) &context->transmit_buffer.buffer[header_offset];
        data_offset = buffer_offset;
        buffer_offset += align_to_cache_line_size (context->path_def.max_message_size);
        api_buffer->data = &context->transmit_buffer.buffer[data_offset];
        freed_sequence_number_offset = buffer_offset;
        buffer_offset += align_to_cache_line_size (sizeof (uint32_t));
        tx_buffer->freed_sequence_number = (uint32_t *) &context->transmit_buffer.buffer[freed_sequence_number_offset];

        /* Set a scatter-gather entry to transmit the maximum message data size, actual length will be set at run time */
        data_sge->lkey = context->transmit_mr->lkey;
        data_sge->addr = (uintptr_t) api_buffer->data;
        data_sge->length =  context->path_def.max_message_size;

        /* Set a scatter-gather entry to transmit the fixed size message header */
        header_sge->lkey = context->transmit_mr->lkey;
        header_sge->addr = (uintptr_t) api_buffer->header;
        header_sge->length = sizeof (message_header);

        /* Set the send work request for the message data.
         * The next pointer is set to the header work-request, so that a ibv_post_send() on the the data_wr
         * sends the data followed by the header. */
        data_wr->wr_id = buffer_index;
        data_wr->sg_list = data_sge;
        data_wr->num_sge = 1;
        data_wr->next = header_wr;
        data_wr->opcode = IBV_WR_RDMA_WRITE;
        data_wr->send_flags = 0;
        data_wr->wr.rdma.rkey = context->slp_connection.remote_attributes.rkey;
        data_wr->wr.rdma.remote_addr = context->slp_connection.remote_attributes.addr + data_offset;

        /* Set the send work request for the message header */
        header_wr->wr_id = buffer_index;
        header_wr->sg_list = header_sge;
        header_wr->num_sge = 1;
        header_wr->next = NULL;
        header_wr->opcode = IBV_WR_RDMA_WRITE;
        header_wr->send_flags = 0;
        if (sizeof (message_header) <= context->message_transmit_qp_max_inline_data)
        {
            header_wr->send_flags |= IBV_SEND_INLINE;
        }
        header_wr->wr.rdma.rkey = context->slp_connection.remote_attributes.rkey;
        header_wr->wr.rdma.remote_addr = context->slp_connection.remote_attributes.addr + header_offset;

        /* Initialise the sequence number management */
        tx_buffer->await_buffer_freed = false;
        tx_buffer->owned_by_application = false;
        tx_buffer->next_transmit_sequence_number = buffer_index + 1;
        tx_buffer->message_not_freed_sequence_number = 0;
        tx_buffer->message_freed_sequence_number = buffer_index + 1;
    }

    context->next_transmit_buffer_index = 0;
    context->message_transmit_cq_pacing = 0;
    context->num_outstanding_completions = 0;
}

/**
 * @detail Perform the first part of the handshake of connection of a transmit communication path by waiting for the
 *         attributes of the remote endpoint, and then transition the Queue-Pair of this endpoint to the ready-to-receive state.
 * @param[in,out] context The transmit message context to complete the initialisation for
 */
void message_transmit_attach_remote_pre_rtr (tx_message_context_handle context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    /* Wait for the attributes of the remote endpoint to be retrieved from SLP */
    get_remote_memory_buffer_from_slp (&context->slp_connection);
    if (context->path_def.tx_checks_memory_buffer_size &&
            (context->slp_connection.local_attributes.size != context->slp_connection.remote_attributes.size))
    {
        fprintf (stderr, "tx_%d has memory buffer size %lu, but rx_%d has size %lu\n",
                 context->path_def.instance, context->slp_connection.local_attributes.size,
                 context->path_def.instance, context->slp_connection.remote_attributes.size);
        fprintf (stderr, "Check --max-msg-size and --num-buffers command line arguments on transmitter and receiver are the same.\n");
        exit (EXIT_FAILURE);
    }

    /* Transition the transmit Queue Pair to the Ready to Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = context->endpoint.port_attributes.active_mtu;
    qp_attr.dest_qp_num = context->slp_connection.remote_attributes.qp_num;
    qp_attr.rq_psn = context->slp_connection.remote_attributes.psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = context->slp_connection.remote_attributes.lid;
    qp_attr.ah_attr.sl = context->path_def.service_level;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = context->path_def.source_port_num;
    rc = ibv_modify_qp (context->message_transmit_qp, &qp_attr,
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
    verify_qp_state (IBV_QPS_RTR, context->message_transmit_qp, "message_transmit_qp", NULL);

    /* Report that this endpoint is ready-to-receive */
    report_local_memory_buffer_rtr_with_slp (&context->slp_connection);
}

/**
 * @detail Perform the second part of the handshake of connection of a transmit communication path by waiting for the
 *         remote endpoint to be ready-to-receive, and then transition the Queue-Pair of this endpoint to the ready-to-send state.
 * @param[in,out] context The transmit message context to complete the initialisation for
 */
void message_transmit_attach_remote_post_rtr (tx_message_context_handle context)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    await_remote_memory_buffer_rtr_from_slp (&context->slp_connection);

    /* Transition the sender Queue Pair to the Ready to Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = context->message_transmit_psn;
    qp_attr.timeout = context->path_def.set_non_default_retry_timeout ?
            context->path_def.retry_timeout : context->endpoint.device_attributes.local_ca_ack_delay;
    qp_attr.retry_cnt = 7; /* Maximum */
    qp_attr.rnr_retry = 0;
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (context->message_transmit_qp, &qp_attr,
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
    verify_qp_state (IBV_QPS_RTS, context->message_transmit_qp, "message_transmit_qp", &context->path_def);

    initialise_transmit_message_buffers (context);
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
    free (context->api_message_buffers);
    free (context->tx_message_buffers);
    release_memory_buffer (&context->transmit_buffer);

    /* Close the Infiniband port */
    close_ib_port_endpoint (&context->endpoint);
    free (context);
}

/**
 * @brief Determine if a message buffer is free, or if a previous message to use the buffer is still in use by the receiver
 * @param[in,out] context The transmit message context to check for the message buffer being free
 * @param[in,out] buffer_index Which message buffer to check for being free
 * @return Returns:
 *         false - the buffer is in use with a message which has not been freed by the receiver.
 *         true  - the buffer is free, and can be used to send a message.
 */
static bool is_buffer_free (tx_message_context_handle context, const uint32_t buffer_index)
{
    tx_message_buffer *const tx_buffer = &context->tx_message_buffers[buffer_index];
    struct ibv_wc wc;
    int num_completions;
    bool buffer_free = false;

    if (tx_buffer->await_buffer_freed)
    {
        const uint32_t sampled_sequence_number = __atomic_load_n (tx_buffer->freed_sequence_number, __ATOMIC_ACQUIRE);

        if (sampled_sequence_number == tx_buffer->message_freed_sequence_number)
        {
            buffer_free = true;
        }
        else
        {
            CHECK_ASSERT (sampled_sequence_number == tx_buffer->message_not_freed_sequence_number);
            if (context->path_def.tx_polls_for_errors)
            {
                /* While waiting for the previous message to be freed, checked if an Infiniband error has occurred */
                num_completions = ibv_poll_cq (context->message_transmit_cq, 1, &wc);
                if (num_completions == 1)
                {
                    if (wc.status == IBV_WC_SUCCESS)
                    {
                        context->num_outstanding_completions--;
                    }
                    else
                    {
                        fprintf (stderr, "Tx %d failed with Infiniband CQ error %s\n",
                                context->path_def.instance, ibv_wc_status_str (wc.status));
                        exit (EXIT_FAILURE);
                    }
                }
                else
                {
                    CHECK_ASSERT (num_completions == 0);
                }
            }
        }

        if (buffer_free)
        {
            tx_buffer->message_not_freed_sequence_number = tx_buffer->message_freed_sequence_number;
            tx_buffer->message_freed_sequence_number = tx_buffer->next_transmit_sequence_number;
            tx_buffer->await_buffer_freed = false;
        }
    }
    else
    {
        buffer_free = true;
    }

    return buffer_free;
}

/**
 * @brief Wait for all messages sent on a communication path to be marked as freed by the receiver
 * @details This is to be used for tests where the transmitter needs to wait for the receiver to have processed all preceding
 *          messages before continuing.
 * @param[in,out] context The transmit message context to wait for the messages to be freed
 */
void await_all_outstanding_messages_freed (tx_message_context_handle context)
{
    uint32_t buffer_index;

    for (buffer_index = 0; buffer_index < context->path_def.num_message_buffers; buffer_index++)
    {
        while (!is_buffer_free (context, buffer_index))
        {
        }
    }
}

/**
 * @brief Attempt to get a message send buffer to populate
 * @details If the next message buffer is still in use, then unable to obtain a buffer
 * @param[in,out] context The transmit context to get the message buffer for
 * @return Returns a pointer to a message buffer, for which the message field can be populated with a message to send.
 *                 Returns NULL if a free buffer is not currently available
 */
tx_api_message_buffer *get_send_buffer_no_wait (tx_message_context_handle context)
{
    tx_api_message_buffer *api_buffer = &context->api_message_buffers[context->next_transmit_buffer_index];
    tx_message_buffer *const tx_buffer = &context->tx_message_buffers[context->next_transmit_buffer_index];

    CHECK_ASSERT (!tx_buffer->owned_by_application);
    if (is_buffer_free (context, context->next_transmit_buffer_index))
    {
        /* Mark the buffer as being prepared by the application, and advance to the next sender buffer index */
        tx_buffer->owned_by_application = true;
        context->next_transmit_buffer_index = (context->next_transmit_buffer_index + 1) % context->path_def.num_message_buffers;
    }
    else
    {
        api_buffer = NULL;
    }

    return api_buffer;
}

/**
 * @brief Get a message send buffer to populate
 * @details If the next message buffer is still in use, then waits with a busy-poll for the receiver to indicate
 *          the message has been freed (which also implies the previous send has completed).
 * @param[in,out] context The transmit context to get the message buffer for
 * @return Returns a pointer to a message buffer, for which the message field can be populated with a message to send
 */
tx_api_message_buffer *get_send_buffer (tx_message_context_handle context)
{
    tx_api_message_buffer *api_buffer;

    do
    {
        api_buffer = get_send_buffer_no_wait (context);
    } while (api_buffer == NULL);

    return api_buffer;
}

/**
 * @brief Send a message with the content which has been populated by the caller
 * @details When this function returns the message has been queued for transmission
 * @param[in,out] api_buffer The message buffer to send, which was previously returned by get_send_buffer()
 *                       and which the message to send has been populated by the caller.
 *                       This function will set the api_buffer->header->sequence_number field
 */
void send_message (const tx_api_message_buffer *const api_buffer)
{
    tx_message_buffer *const tx_buffer = &api_buffer->context->tx_message_buffers[api_buffer->buffer_index];
    struct ibv_send_wr *bad_wr = NULL;
    int rc;

    CHECK_ASSERT ((tx_buffer->owned_by_application) &&
                  (api_buffer->header->message_length <= api_buffer->context->path_def.max_message_size));
    tx_buffer->owned_by_application = false;

    /* Wait for the message send work request completion, which is signalled on one message in every num_message_buffers */
    if (api_buffer->context->message_transmit_cq_pacing == api_buffer->context->path_def.num_message_buffers)
    {
        struct ibv_wc wc;
        int num_completions;

        if (api_buffer->context->num_outstanding_completions > 0)
        {
            do
            {
                num_completions = ibv_poll_cq (api_buffer->context->message_transmit_cq, 1, &wc);
            } while (num_completions == 0);
            CHECK_ASSERT ((num_completions == 1) && (wc.status == IBV_WC_SUCCESS));
            api_buffer->context->num_outstanding_completions--;
        }
        api_buffer->context->message_transmit_cq_pacing = 0;
    }

    /* Complete the message to be sent */
    __atomic_store_n (&api_buffer->header->sequence_number, tx_buffer->next_transmit_sequence_number, __ATOMIC_RELEASE);
    tx_buffer->transmit_sges[MESSAGE_DATA_WQE_INDEX].length = api_buffer->header->message_length;
    if (api_buffer->context->message_transmit_cq_pacing == 0)
    {
        tx_buffer->transmit_wrs[MESSAGE_HEADER_WQE_INDEX].send_flags |= IBV_SEND_SIGNALED;
        api_buffer->context->num_outstanding_completions++;
    }
    else
    {
        tx_buffer->transmit_wrs[MESSAGE_HEADER_WQE_INDEX].send_flags &= ~IBV_SEND_SIGNALED;
    }
    api_buffer->context->message_transmit_cq_pacing++;

    if (tx_buffer->transmit_wrs[MESSAGE_DATA_WQE_INDEX].sg_list->length > 0)
    {
        /* Start the transfer for the data followed by the header */
        if (tx_buffer->transmit_wrs[MESSAGE_DATA_WQE_INDEX].sg_list->length <=
                api_buffer->context->message_transmit_qp_max_inline_data)
        {
            tx_buffer->transmit_wrs[MESSAGE_DATA_WQE_INDEX].send_flags |= IBV_SEND_INLINE;
        }
        else
        {
            tx_buffer->transmit_wrs[MESSAGE_DATA_WQE_INDEX].send_flags &= ~IBV_SEND_INLINE;
        }
        rc =  ibv_post_send (api_buffer->context->message_transmit_qp, &tx_buffer->transmit_wrs[MESSAGE_DATA_WQE_INDEX], &bad_wr);
        CHECK_ASSERT (rc == 0);
    }
    else
    {
        /* Start the transfer for the header */
        rc =  ibv_post_send (api_buffer->context->message_transmit_qp,
                &tx_buffer->transmit_wrs[MESSAGE_HEADER_WQE_INDEX], &bad_wr);
        CHECK_ASSERT (rc == 0);
    }

    tx_buffer->await_buffer_freed = true;

    /* Advance to the next sequence number for this buffer */
    tx_buffer->next_transmit_sequence_number += api_buffer->context->path_def.num_message_buffers;
}


/**
 * @brief Wait for all queued DMA for a transmit path to complete.
 * @param[in,out] context The transmit path to wait for the DMA to complete
 */
void flush_transmit_dma (tx_message_context_handle context)
{
    int rc;
    struct ibv_wc wc;
    int num_completions;
    struct ibv_send_wr *bad_wr = NULL;
    struct ibv_send_wr flush_wr;

    /* Since completion queue pacing is used to only signal completion on one of num_message_buffers there may be
     * queued transmit work-requests for which completion isn't signalled.
     * Therefore, insert a zero-length work request with completion signalled, to be able to determine when any prior
     * work requests with un-signalled completion have completed. */
    memset (&flush_wr, 0, sizeof (flush_wr));
    flush_wr.sg_list = NULL;
    flush_wr.num_sge = 0;
    flush_wr.next = NULL;
    flush_wr.opcode = IBV_WR_RDMA_WRITE;
    flush_wr.send_flags = IBV_SEND_SIGNALED;
    rc = ibv_post_send (context->message_transmit_qp, &flush_wr, &bad_wr);
    CHECK_ASSERT (rc == 0);
    context->num_outstanding_completions++;

    /* Wait for outstanding work-queue completions. */
    while (context->num_outstanding_completions > 0)
    {
        do
        {
            num_completions = ibv_poll_cq (context->message_transmit_cq, 1, &wc);
        } while (num_completions == 0);
        CHECK_ASSERT (num_completions == 1);
        context->num_outstanding_completions--;
    }
}
