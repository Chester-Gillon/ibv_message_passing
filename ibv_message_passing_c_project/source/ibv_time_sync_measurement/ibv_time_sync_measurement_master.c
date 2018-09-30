/*
 * @file ibv_time_sync_measurement_master.c
 * @date 16 Sep 2018
 * @author Chester Gillon
 * @brief The master program for time synchronisation measurement.
 */

#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <signal.h>

#include <infiniband/verbs.h>
#include <slp.h>
#include <sys/mman.h>

#include "ibv_message_bw_interface.h"
#include "ibv_time_sync_measurement_messages.h"


/** Contains the time synchronisation measurements for one range of round trip delay. */
typedef struct
{
    /** The number of samples for this range of round trip delay */
    uint64_t num_samples;
    /** The min and max round trip delay times for the measurements */
    int64_t min_round_trip_delay_ns;
    int64_t max_round_trip_delay_ns;
    /** The min and max slave-to-master time synchronisation offset for the round trip times */
    int64_t min_slave_to_master_time_offset_ns;
    int64_t max_slave_to_master_time_offset_ns;
} time_sync_interval_bin_results;


/** The number of microseconds of round trip delay over which time synchronisation measures are recorded */
#define NUM_ROUND_TRIP_DELAY_MICROSEC_BINS 100

/** Contains the time synchronisation measurements made over a one second interval */
typedef struct
{
    /* Results binned by the round trip delay */
    time_sync_interval_bin_results us_binned_results[NUM_ROUND_TRIP_DELAY_MICROSEC_BINS];
    /* Results for when the round trip delay exceeds the bins */
    time_sync_interval_bin_results outlier_results;
} time_sync_interval_results;


/** Allow the test to collect time synchronisation measurements for up to one day */
#define MAX_MEASUREMENT_INTERVALS (24 * 60 * 60)


/** Used to store the results of all time synchronisation measurement intervals */
static time_sync_interval_results test_iteration_results[MAX_MEASUREMENT_INTERVALS];
static uint32_t num_test_iterations;


/** Set from a signal handle to tell the main thread to stop the time synchronisation measurement. */
static volatile bool stop_measurement;

/**
 * @brief Signal handler to request measurement of time synchronisation is stopped
 */
static void stop_measurement_handler (const int sig)
{
    stop_measurement = true;
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
 * @brief Get an estimate of the the time synchronisation error between the master and slave.
 * @details It is assumed that the slave reads its time at the mid point of the round trip delay.
 *          The larger the round trip delay, the more uncertainty their is in the reported time synchronisation error.
 * @param[in] master_start_time The master time at which the time request was sent to the slave.
 * @param[in] master_end_time The master time at which the time reply was received from the slave.
 * @param[in] slave_time The time received from the slave.
 * @return The offset between the master and slave time in nanoseconds.
 */
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


/**
 * @brief Perform one iteration of the time synchronisation between the master and slave, over a one second interval
 * @param[in] communication_context Used to receive messages from the slave
 * @param[in] path_to_slave Used to send messages to the slave
 * @param[out] results The time synchronisation measurements results over the interval.
 */
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

        rc = clock_gettime (CLOCK_REALTIME, &master_end_time);
        CHECK_ASSERT (rx_buffer->header->message_id == TIME_SYNC_TIME_REPLY);
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


/**
 * @brief Write the time synchronisation measurements for all test iterations to a CSV file
 * @parma[in,out] results_file The CSV file to write the measurement results to
 */
static void write_results_file (FILE *const results_file)
{
    /* Write column headers */
    fprintf (results_file, "Iteration,Round-trip delay bin,Num samples,Min round trip delay (ns),Max round trip delay (ns)"
            ",Min slave-to-master time offset (ns),Max slave-to-master time offset (ns)\n");

    for (uint32_t iteration = 0; iteration < num_test_iterations; iteration++)
    {
        const time_sync_interval_results *const results = &test_iteration_results[iteration];

        for (uint32_t bin_index = 0; bin_index < NUM_ROUND_TRIP_DELAY_MICROSEC_BINS; bin_index++)
        {
            const time_sync_interval_bin_results *const bin_results = &results->us_binned_results[bin_index];

            if (bin_results->num_samples > 0)
            {
                fprintf (results_file, "%" PRIu32 ",%d <= us < %d,%" PRIi64 ",%" PRIi64 ",%" PRIi64 ",%" PRIi64 ",%" PRIi64 "\n",
                        iteration, bin_index, bin_index + 1, bin_results->num_samples,
                        bin_results->min_round_trip_delay_ns, bin_results->max_round_trip_delay_ns,
                        bin_results->min_slave_to_master_time_offset_ns, bin_results->max_slave_to_master_time_offset_ns);

            }
        }

        if (results->outlier_results.num_samples > 0)
        {
            fprintf (results_file, "%" PRIu32 ",>= %d us,%" PRIi64 ",%" PRIi64 ",%" PRIi64 ",%" PRIi64 ",%" PRIi64 "\n",
                    iteration, NUM_ROUND_TRIP_DELAY_MICROSEC_BINS, results->outlier_results.num_samples,
                    results->outlier_results.min_round_trip_delay_ns, results->outlier_results.max_round_trip_delay_ns,
                    results->outlier_results.min_slave_to_master_time_offset_ns,
                    results->outlier_results.max_slave_to_master_time_offset_ns);
        }
    }
}


int main (int argc, char *argv[])
{
    communication_context_handle communication_context;
    tx_message_context_handle path_to_slave;
    tx_api_message_buffer *tx_buffer;
    rx_api_message_buffer *rx_buffer;
    int rc;
    FILE *results_file;

    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s <result_csv_filename>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    if (rc != 0)
    {
        perror ("mlockall");
        exit (EXIT_FAILURE);
    }

    const char *results_filename = argv[1];

    results_file = fopen (results_filename, "w");
    if (results_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", results_filename);
        exit (EXIT_FAILURE);
    }

    /* Initialise communication paths */
    register_time_sync_measurement_messages ();
    communication_context = communication_context_initialise (TIME_SYNC_MASTER_NODE_ID);
    path_to_slave = get_tx_path_handle (communication_context, TIME_SYNC_SLAVE_NODE_ID, TIME_SYNC_MASTER_TO_SLAVE_PATH);

    /* Install a signal handler to allow a request to stop transmission */
    struct sigaction action;

    printf ("Press Ctrl-C to stop the time synchronisation measurement\n");
    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_measurement_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    check_assert (rc == 0, "sigaction");

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

    /* Perform the test until told to stop */
    for (num_test_iterations = 0; (!stop_measurement) && (num_test_iterations < MAX_MEASUREMENT_INTERVALS); num_test_iterations++)
    {
        perform_time_sync_measurement_iteration (communication_context, path_to_slave,
                &test_iteration_results[num_test_iterations]);
    }

    /* Tell the slave the test is complete */
    tx_buffer = get_send_buffer (path_to_slave);
    tx_buffer->header->message_id = TIME_SYNC_TEST_COMPLETE;
    tx_buffer->header->message_length = 0;
    tx_buffer->header->source_instance = 0;
    send_message (tx_buffer);

    communication_context_finalise (communication_context);

    write_results_file (results_file);
    fclose (results_file);

    return EXIT_SUCCESS;
}
