/*
 * @file ibv_time_dma_flush.c
 * @date 31 Mar 2019
 * @author Chester Gillon
 * @brief Program to time the flush_transmit_dma() operation, with and without first waiting to receive messages.
 * @details This is only run on a single computer, with messages looped back through two Infiniband ports.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>

#include "ibv_message_bw_interface.h"


/* Define a large amount of data to queue for transmission to give a measurable delay for the DMA to complete */
#define MESSAGE_SIZE_BYTES (128 * 1024 * 1024)
#define NUM_MESSAGE_BUFFERS 20


/**
 * @brief Return the elapsed time in nanoseconds between two time stamps
 * @param[in] start_time The start time for the elapsed duration
 * @param[in] end_time The end time for the elapsed duration
 * @return Returns the elapsed time in nanoseconds
 */
static int64_t get_elapsed_ns (const struct timespec *const start_time, const struct timespec *const end_time)
{
    const int64_t nsecs_per_sec = 1000000000;
    const int64_t start_time_ns = (start_time->tv_sec * nsecs_per_sec) + start_time->tv_nsec;
    const int64_t end_time_ns = (end_time->tv_sec * nsecs_per_sec) + end_time->tv_nsec;

    return end_time_ns - start_time_ns;
}


/**
 * @brief Queue the transmit messages for the path
 * @details Since this program is only interested in the DMA transmit timing,
 *          all messages are the maximum size, with no explicit content.
 * @param[in] tx_path The path to transmit the messages on.
 */
static void queue_transmit_messages (tx_message_context_handle const tx_path)
{
    for (int buffer_index = 0; buffer_index < NUM_MESSAGE_BUFFERS; buffer_index++)
    {
        tx_api_message_buffer *const tx_buffer = get_send_buffer (tx_path);

        tx_buffer->header->message_length = MESSAGE_SIZE_BYTES;
        send_message (tx_buffer);
    }
}


/**
 * @brief Await the receipt of all messages queued by queue_transmit_messages()
 * @param[in] communication_context Used to wait for the message receipt
 * @return The number of nanoseconds spent waiting for receipt of all messages
 */
static int64_t time_message_receipt (communication_context_handle const communication_context)
{
    int rc;
    struct timespec start_time;
    struct timespec end_time;

    rc = clock_gettime (CLOCK_MONOTONIC, &start_time);
    CHECK_ASSERT (rc == 0);

    for (int buffer_index = 0; buffer_index < NUM_MESSAGE_BUFFERS; buffer_index++)
    {
        rx_api_message_buffer *const rx_buffer = await_any_rx_message (communication_context);

        CHECK_ASSERT (rx_buffer->header->message_length == MESSAGE_SIZE_BYTES);
        free_message (rx_buffer);
    }

    rc = clock_gettime (CLOCK_MONOTONIC, &end_time);
    CHECK_ASSERT (rc == 0);

    return get_elapsed_ns (&start_time, &end_time);
}


/*
 * @brief Time the flush_transmit_dma() operation
 * @param[in] tx_path Which transmit path to flush the transmit DMA for
 * @return The number of nanoseconds spent waiting for the transmit DMA to flush
 */
static int64_t time_dma_flush (tx_message_context_handle const tx_path)
{
    int rc;
    struct timespec start_time;
    struct timespec end_time;

    rc = clock_gettime (CLOCK_MONOTONIC, &start_time);
    CHECK_ASSERT (rc == 0);

    flush_transmit_dma (tx_path);

    rc = clock_gettime (CLOCK_MONOTONIC, &end_time);
    CHECK_ASSERT (rc == 0);

    return get_elapsed_ns (&start_time, &end_time);
}


int main (int argc, char *argv[])
{
    communication_context_handle communication_context;
    tx_message_context_handle tx_path;
    int64_t rx_time_ns;
    int64_t dma_flush_time_ns;

    /* Register a single path using Infiniband ports on the local host */
    communication_path_definition path_def =
    {
        .allocation_type = BUFFER_ALLOCATION_SHARED_MEMORY,
        .tx_checks_memory_buffer_size = true,
        .tx_polls_for_errors = false,
        .set_non_default_retry_timeout = false,
        .source_node = 0,
        .destination_node = 0,
        .instance = 0,
        .source_ib_device = "mlx4_0",
        .source_port_num = 1,
        .destination_ib_device = "mlx4_0",
        .destination_port_num = 2,
        .service_level = DEFAULT_SERVICE_LEVEL,
        .max_message_size = MESSAGE_SIZE_BYTES,
        .num_message_buffers = NUM_MESSAGE_BUFFERS
    };
    register_path_definition (&path_def);

    /* Initialise the path. As only a single path, using two ports on the local board, the node IDs and instance
     * numbers are all zeros. */
    communication_context = communication_context_initialise (0);
    tx_path = get_tx_path_handle (communication_context, 0, 0);

    /* Flush the DMA then wait for the receive messages.
     * If the flush_transmit_dma() operation is correctly waiting for all queued messages to complete transmission
     * then the message reception shouldn't have to wait. */
    queue_transmit_messages (tx_path);
    dma_flush_time_ns = time_dma_flush (tx_path);
    rx_time_ns = time_message_receipt (communication_context);
    printf ("dma_flush_time=%.6f secs, followed by rx_time=%.6f\n", dma_flush_time_ns / 1E9, rx_time_ns / 1E9);

    /* Wait for the receive messages and then flush the DMA.
     * The flush_transmit_dma() operation shouldn't have to wait. */
    queue_transmit_messages (tx_path);
    rx_time_ns = time_message_receipt (communication_context);
    dma_flush_time_ns = time_dma_flush (tx_path);
    printf ("rx_time=%.6f secs, followed by dma_flush_time=%.6f\n", rx_time_ns / 1E9, dma_flush_time_ns / 1E9);

    communication_context_finalise (communication_context);

    return EXIT_SUCCESS;
}
