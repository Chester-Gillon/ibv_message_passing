/*
 * @file ibv_message_latency_master.c
 * @date 18 Nov 2018
 * @author Chester Gillon
 * @brief The master for the latency measurement tests
 * @details Latency is measured on one or more paths from the master to slave which use same Infiniband ports.
 *          The master starts transmission of messages on the parallel paths as quickly as possibly, and the slave times the
 *          receipt of the message on each path to determine the latency.
 *          PTP is used to synchronise the time between the master and slave PCs to measure latency.
 *
 *          The number and Infiniband service level of the paths is specified from command line options.
 *          The Service Level may be changed to see if the SL2VL mapping and VL arbitration tables can be set to give
 *          a different priority to each path.
 */

#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>
#include <sys/mman.h>

#include "ibv_message_bw_interface.h"
#include "ibv_message_latency_messages.h"


/* Contains the message latency measurements for one iteration.
 * All times are CLOCK_REALTIME, which relies upon PTP to synchronise the time between the master and slave PCs. */
typedef struct
{
    /* Times on the master prior to calling send_message() */
    struct timespec message_tx_times[MESSAGE_LATENCY_MAX_PARALLEL_PATHS];
    /* Time on the slave when each message was received */
    struct timespec message_rx_times[MESSAGE_LATENCY_MAX_PARALLEL_PATHS];
    /* Time on master when the MESSAGE_LATENCY_ACK_MSG_ID has been received from the slave,
     * for comparing against the other times. */
    struct timespec iteration_complete_time;
} interation_latency_measurement;


/* Latency measurements over a number of repeats, to check for consistency */
#define NUM_LATENCY_MEASUREMENTS 100
static interation_latency_measurement latency_measurements[NUM_LATENCY_MEASUREMENTS];


/**
 * @details Run one latency measurement iteration, but sending a message on each communication path and awaiting the message
 *          receive times from the slave.
 * @param[in/out] communication_context Used to receive the ACK message from the slave.
 * @param[in] num_data_paths The number of parallel paths to the slave.
 * @param[in/out] paths_to_slave The parallel communication paths to the slave to send the latency messages on.
 * @param[out] result The latency measurement times for the iteration.
 */
static void run_latency_measurement_iteration (communication_context_handle const communication_context,
                                               const uint32_t num_data_paths,
                                               tx_message_context_handle paths_to_slave[const num_data_paths],
                                               interation_latency_measurement *const result)
{
    int rc;
    uint32_t path_index;
    tx_api_message_buffer *tx_buffers[MESSAGE_LATENCY_MAX_PARALLEL_PATHS];
    rx_api_message_buffer *rx_buffer;

    /* Prepare the transmit buffers */
    for (path_index = 0; path_index < num_data_paths; path_index++)
    {
        tx_buffers[path_index] = get_send_buffer (paths_to_slave[path_index]);
        tx_buffers[path_index]->header->source_instance = path_index;
        tx_buffers[path_index]->header->message_id = MESSAGE_LATENCY_DATA_MSG_ID;
        tx_buffers[path_index]->header->message_length = MESSAGE_LATENCY_DATA_SIZE_BYTES;
    }

    /* Send the data messages to the slave */
    for (path_index = 0; path_index < num_data_paths; path_index++)
    {
        rc = clock_gettime (CLOCK_REALTIME, &result->message_tx_times[path_index]);
        send_message (tx_buffers[path_index]);
        CHECK_ASSERT (rc == 0);
    }

    /* Await the ACK from the slave with the message receive times */
    rx_buffer = await_any_rx_message (communication_context);
    rc = clock_gettime (CLOCK_REALTIME, &result->iteration_complete_time);
    const latency_ack_msg *const latency_ack = rx_buffer->data;
    CHECK_ASSERT (rc == 0);
    CHECK_ASSERT (rx_buffer->header->message_id == MESSAGE_LATENCY_ACK_MSG_ID);
    (void) memcpy (result->message_rx_times, latency_ack->message_rx_times, sizeof (result->message_rx_times));
    free_message (rx_buffer);
}


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
 * @brief Write the latency measurement results for all iterations to a CSV file
 * @param[in/out] results_file The file to write to.
 * @param[in] num_data_paths The number of parallel paths to the slave.
 * @param[in] service_levels The Infiniband Service Level for each path to the slave, used to write the CSV column headers.
 */
static void write_results_csv_file (FILE *const results_file, const uint32_t num_data_paths,
                                    const uint32_t service_levels[const num_data_paths])
{
    uint32_t path_index;
    uint32_t iteration;

    for (path_index = 0; path_index < num_data_paths; path_index++)
    {
        fprintf (results_file, "SL%" PRIu32 " latency (ns),", service_levels[path_index]);
    }
    fprintf (results_file, "Overall latency (ns)\n");

    for (iteration = 0; iteration < NUM_LATENCY_MEASUREMENTS; iteration++)
    {
        const interation_latency_measurement *const result = &latency_measurements[iteration];

        for (path_index = 0; path_index < num_data_paths; path_index++)
        {
            fprintf (results_file, "%" PRIi64 ",",
                    get_elapsed_ns (&result->message_tx_times[path_index], &result->message_rx_times[path_index]));
        }
        fprintf (results_file, "%" PRIi64 "\n",
                get_elapsed_ns (&result->message_tx_times[0], &result->iteration_complete_time));
    }
}


int main (int argc, char *argv[])
{
    int rc;
    communication_context_handle communication_context;
    uint32_t path_index;
    rx_api_message_buffer *rx_buffer;
    tx_api_message_buffer *tx_buffer;
    uint32_t service_levels[MESSAGE_LATENCY_MAX_PARALLEL_PATHS];
    tx_message_context_handle paths_to_slave[MESSAGE_LATENCY_MAX_PARALLEL_PATHS] = {NULL};
    interation_latency_measurement result;
    uint32_t iteration;
    FILE *results_file;

    if (argc != 3)
    {
        fprintf (stderr, "Usage: %s <comma_delimited_service_levels> <csv_result_filename>\n", argv[0]);
        exit (EXIT_FAILURE);
    }
    const char *service_levels_arg = argv[1];
    const char *results_filename = argv[2];

    results_file = fopen (results_filename, "w");
    if (results_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", results_filename);
        exit (EXIT_FAILURE);
    }

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    if (rc != 0)
    {
        perror ("mlockall");
        exit (EXIT_FAILURE);
    }

    /* Initialise the communication paths */
    const uint32_t num_data_paths = register_message_latency_messages (service_levels_arg, service_levels);
    communication_context = communication_context_initialise (MESSAGE_LATENCY_MASTER_NODE_ID);
    for (path_index = 0; path_index < num_data_paths; path_index++)
    {
        paths_to_slave[path_index] = get_tx_path_handle (communication_context, MESSAGE_LATENCY_SLAVE_NODE_ID,
                MASTER_TEST_MESSAGES_FIRST_PATH + path_index);
    }

    /* Perform one warm-up iteration to ensure the slave is ready */
    run_latency_measurement_iteration (communication_context, num_data_paths, paths_to_slave, &result);

    /* Run the latency measurement iterations */
    for (iteration = 0; iteration < NUM_LATENCY_MEASUREMENTS; iteration++)
    {
        run_latency_measurement_iteration (communication_context, num_data_paths, paths_to_slave,
                &latency_measurements[iteration]);
    }

    /* Tell the slave to exit */
    for (path_index = 0; path_index < num_data_paths; path_index++)
    {
        tx_buffer = get_send_buffer (paths_to_slave[path_index]);
        tx_buffer->header->message_id = MESSAGE_LATENCY_TEST_COMPLETE_MSG_ID;
        tx_buffer->header->message_length = 0;
        send_message (tx_buffer);
    }
    rx_buffer = await_any_rx_message (communication_context);
    free_message (rx_buffer);

    communication_context_finalise (communication_context);

    /* Write the results */
    write_results_csv_file (results_file, num_data_paths, service_levels);
    fclose (results_file);

    return EXIT_SUCCESS;
}
