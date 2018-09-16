/*
 * @file ibv_time_sync_measurement_master.c
 * @date 16 Sep 2018
 * @author Chester Gillon
 * @brief The master program for time synchronisation measurement
 */

#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>
#include <sys/mman.h>

#include "ibv_message_bw_interface.h"
#include "ibv_time_sync_measurement_messages.h"


typedef struct
{
    uint64_t num_samples;
    int64_t min_round_trip_delay_ns;
    int64_t max_round_trip_delay_ns;
    int64_t min_slave_to_master_time_offset_ns;
    int64_t max_slave_to_master_time_offset_ns;
} time_sync_interval_bin_results;


#define NUM_ROUND_TRIP_DELAY_MICROSEC_BINS 100

typedef struct
{
    /* Results binned by the round trip delay */
    time_sync_interval_bin_results us_binned_results[NUM_ROUND_TRIP_DELAY_MICROSEC_BINS];
    /* Results for when the round trip delay exceeds the bins */
    time_sync_interval_bin_results outlier_results;
}time_sync_interval_results;


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


static int64_t get_time_sync_error (const struct timespec *const master_start_time, const struct timespec *const master_end_time,
                                    const struct timespec *const slave_time)
{
    const int64_t nsecs_per_sec = 1000000000;
    const int64_t master_start_time_ns = (master_start_time->tv_sec * nsecs_per_sec) + master_start_time->tv_nsec;
    const int64_t master_end_time_ns = (master_end_time->tv_sec * nsecs_per_sec) + master_end_time->tv_nsec;
    const int64_t round_trip_time_ns = master_end_time_ns - master_start_time_ns;
    const int64_t slave_time_ns = (slave_time->tv_sec * nsecs_per_sec) + slave_time->tv_nsec;
    const int64_t master_mid_time_ns = master_start_time_ns + (round_trip_time_ns / 2);

    return slave_time_ns - master_mid_time_ns;
}


static void perform_time_sync_measurement_iteration (communication_context_handle const communication_context,
                                                     tx_message_context_handle const path_to_slave,
                                                     time_sync_interval_results *const results)
{
    int rc;
    struct timespec iteration_start_time;
    struct timespec master_start_time;
    struct timespec master_end_time;
    const int64_t measurement_interval_ns = 1000000000;

    memset (results, 0, sizeof (*results));

    rc = clock_gettime (CLOCK_REALTIME, &iteration_start_time);
    CHECK_ASSERT (rc == 0);

    do
    {
        tx_api_message_buffer *const tx_buffer = get_send_buffer (path_to_slave);

        tx_buffer->header->message_id = TIME_SYNC_TIME_REQUEST;
        tx_buffer->header->message_length = 0;
        tx_buffer->header->source_instance = 0;
        rc = clock_gettime (CLOCK_REALTIME, &master_start_time);
        CHECK_ASSERT (rc == 0);
        send_message (tx_buffer);

        rx_api_message_buffer *const rx_buffer = await_any_rx_message (communication_context);
        const slave_current_time_msg *const slave_time_msg = rx_buffer->data;

        CHECK_ASSERT (rx_buffer->header->message_id == TIME_SYNC_TIME_REPLY);
        rc = clock_gettime (CLOCK_REALTIME, &master_end_time);
        CHECK_ASSERT (rc == 0);

        const int64_t round_trip_delay_ns = get_elapsed_ns (&master_start_time, &master_end_time);
        const int64_t slave_to_master_time_offset_ns = get_time_sync_error (&master_start_time, &master_end_time,
                &slave_time_msg->slave_current_time);

        free_message (rx_buffer);

        const int64_t bin_index = round_trip_delay_ns / 1000;
        time_sync_interval_bin_results *const bin_results = (bin_index >= 0) && (bin_index < NUM_ROUND_TRIP_DELAY_MICROSEC_BINS)
                ? &results->us_binned_results[bin_index] : &results->outlier_results;

        if (bin_results->num_samples == 0)
        {
            bin_results->min_round_trip_delay_ns = round_trip_delay_ns;
            bin_results->max_round_trip_delay_ns = round_trip_delay_ns;
            bin_results->min_slave_to_master_time_offset_ns = slave_to_master_time_offset_ns;
            bin_results->max_slave_to_master_time_offset_ns = slave_to_master_time_offset_ns;
        }
        else
        {
            if (round_trip_delay_ns < bin_results->min_round_trip_delay_ns)
            {
                bin_results->min_round_trip_delay_ns = round_trip_delay_ns;
            }
            if (round_trip_delay_ns > bin_results->max_round_trip_delay_ns)
            {
                bin_results->max_round_trip_delay_ns = round_trip_delay_ns;
            }
            if (slave_to_master_time_offset_ns < bin_results->min_slave_to_master_time_offset_ns)
            {
                bin_results->min_slave_to_master_time_offset_ns = slave_to_master_time_offset_ns;
            }
            if (slave_to_master_time_offset_ns > bin_results->max_slave_to_master_time_offset_ns)
            {
                bin_results->max_slave_to_master_time_offset_ns = slave_to_master_time_offset_ns;
            }
        }
        bin_results->num_samples++;
    } while (get_elapsed_ns (&iteration_start_time, &master_end_time) < measurement_interval_ns);
}


int main (int argc, char *argv[])
{
    communication_context_handle communication_context;
    tx_message_context_handle path_to_slave;
    tx_api_message_buffer *tx_buffer;
    rx_api_message_buffer *rx_buffer;
    time_sync_interval_results results;
    int rc;

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    if (rc != 0)
    {
        perror ("mlockall");
        exit (EXIT_FAILURE);
    }

    /* Initialise communication paths */
    register_time_sync_measurement_messages ();
    communication_context = communication_context_initialise (TIME_SYNC_MASTER_NODE_ID);
    path_to_slave = get_tx_path_handle (communication_context, TIME_SYNC_SLAVE_NODE_ID, TIME_SYNC_MASTER_TO_SLAVE_PATH);

    /* Tell the slave we are ready */
    tx_buffer = get_send_buffer (path_to_slave);
    tx_buffer->header->message_id = TIME_SYNC_MASTER_READY_MSG_ID;
    tx_buffer->header->message_length = 0;
    tx_buffer->header->source_instance = 0;
    send_message (tx_buffer);

    /* Wait for the slave to be ready */
    rx_buffer = await_any_rx_message (communication_context);
    CHECK_ASSERT (rx_buffer->header->message_id == TIME_SYNC_SLAVE_READY_MSG_ID);
    free_message (rx_buffer);

    /* Perform the test */
    perform_time_sync_measurement_iteration (communication_context, path_to_slave, &results);

    /* Tell the slave the test is complete */
    tx_buffer = get_send_buffer (path_to_slave);
    tx_buffer->header->message_id = TIME_SYNC_TEST_COMPLETE;
    tx_buffer->header->message_length = 0;
    tx_buffer->header->source_instance = 0;
    send_message (tx_buffer);

    communication_context_finalise (communication_context);

    for (int bin_index = 0; bin_index < NUM_ROUND_TRIP_DELAY_MICROSEC_BINS; bin_index++)
    {
        time_sync_interval_bin_results *const bin_results = &results.us_binned_results[bin_index];
        char prefix[64];

        snprintf (prefix, sizeof (prefix), "[%d <= us < %d]", bin_index, bin_index + 1);
        if (bin_results->num_samples > 0)
        {
            printf ("%s num_samples=%" PRIu64 "\n", prefix, bin_results->num_samples);
            printf ("%s min_round_trip_delay_ns=%" PRIi64 "\n", prefix, bin_results->min_round_trip_delay_ns);
            printf ("%s max_round_trip_delay_ns=%" PRIi64 "\n", prefix, bin_results->max_round_trip_delay_ns);
            printf ("%s min_slave_to_master_time_offset_ns=%" PRIi64 "\n", prefix, bin_results->min_slave_to_master_time_offset_ns);
            printf ("%s max_slave_to_master_time_offset_ns=%" PRIi64 "\n", prefix, bin_results->max_slave_to_master_time_offset_ns);
        }
    }

    if (results.outlier_results.num_samples > 0)
    {
        printf ("outlier num_samples=%" PRIu64 "\n", results.outlier_results.num_samples);
        printf ("outlier min_round_trip_delay_ns=%" PRIi64 "\n", results.outlier_results.min_round_trip_delay_ns);
        printf ("outlier max_round_trip_delay_ns=%" PRIi64 "\n", results.outlier_results.max_round_trip_delay_ns);
        printf ("outlier min_slave_to_master_time_offset_ns=%" PRIi64 "\n", results.outlier_results.min_slave_to_master_time_offset_ns);
        printf ("outlier max_slave_to_master_time_offset_ns=%" PRIi64 "\n", results.outlier_results.max_slave_to_master_time_offset_ns);

    }

    return EXIT_SUCCESS;
}
