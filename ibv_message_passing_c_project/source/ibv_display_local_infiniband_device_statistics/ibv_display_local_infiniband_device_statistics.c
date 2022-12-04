/*
 * @file ibv_display_local_infiniband_device_statistics.c
 * @date 25 Dec 2020
 * @author Chester Gillon
 * @brief Display at regular intervals statistics for all local Infiniband devices.
 * @details This reads the statistics counters from the sysfs files, rather than using Infiniband MAD, so that can work if
 *          a Mellanox VPI HCA is configured with an Infiniband or Ethernet link-layer.
 *
 *          To reduce the amount of out, counters are only displayed with non-zero values.
 *
 *          The delta between adjacent samples is shown, to highlight which counters are changing.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdarg.h>
#include <string.h>

#include <limits.h>
#include <sys/types.h>
#include <dirent.h>
#include <signal.h>

#include <infiniband/verbs.h>

/** The statistics sample for one counter */
typedef struct
{
    /* The filename of the counter read */
    char filename[PATH_MAX];
    /* The pathname of the counter */
    char pathname[PATH_MAX];
    /* Name of underlying kernel IB device, eg "mthca0" */
    char device_name[IBV_SYSFS_NAME_MAX];
    /* When true the counter is per-port, when false the counter is per-device */
    bool per_port;
    /* The port number for the counter, valid when per_port is true */
    uint8_t port_number;
    /** Value of the counter for the current sample */
    uint64_t current_value;
    /** Value of the counter for the previous sample */
    uint64_t previous_value;
} counter_sample_t;


/** The period in seconds between collecting Infiniband statistics */
#define STATISTICS_COLLECTION_PERIOD_SECS 10


/** Array of counters. Allocated at initialisation */
static uint32_t num_counters;
static uint32_t counters_array_size;
static counter_sample_t *counters;


/** Set from a signal handler to request that the statistics collection is stopped */
static volatile bool exit_requested;


/**
 * @brief Signal handler to request collection of statistics is stopped
 */
static void stop_statistics_collection_handler (const int sig)
{
    exit_requested = true;
}


/**
 * @brief Abort the program if an assertion fails, after displaying a message
 * @param[in] assertion Should be true to allow the program to continue.
 * @param[in] format printf style format string for error message.
 * @param[in] ... printf arguments
 */
static void check_assert (const bool assertion, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
static void check_assert (const bool assertion, const char *format, ...)
{
    if (!assertion)
    {
        va_list args;

        va_start (args, format);
        vfprintf (stderr, format, args);
        va_end (args);
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Determine if a statistics counter is supported
 * @details A counter is supported if can read a value from the statistics counter file.
 *          This function was written since with a QLogic FastLinQ QL41000 Series device with the AlmaLinux 8.7
 *          in-box driver it was found that statistic counter files are present, but return EINVAL attempting
 *          to read the counters.
 * @param[in] counter The counter to determine if is supported
 * @return Returns true is the counter is supported
 */
static bool validate_counter_supported (const counter_sample_t *const counter)
{
    FILE *counter_file;
    int num_read;
    uint64_t current_value;
    bool counter_supported = false;

    counter_file = fopen (counter->pathname, "r");
    if (counter_file != NULL)
    {
        num_read = fscanf (counter_file, "%" SCNu64, &current_value);
        counter_supported = (num_read == 1);
        (void) fclose (counter_file);
    }

    return counter_supported;
}


/**
 * @brief Add the definitions of all statistics counter files in one directory
 * @param[in] counters_dir The directory to add the statistics counters for
 * @param[in/out] counter Used to build the counter definition. On entry identifies the device and port for counters_dir
 */
static void add_counters_in_directory (const char *const counters_dir, counter_sample_t *const counter)
{
    DIR *dir;
    struct dirent *entry;
    uint32_t num_supported_counters_in_dir = 0;
    uint32_t total_counters_in_dir = 0;
    const uint32_t array_grow_size = 16;

    dir = opendir (counters_dir);
    if (dir != NULL)
    {
        entry = readdir (dir);
        while (entry != NULL)
        {
            /* All regular files are counters, except for "lifespan" which is how often the hw_counters are updated */
            if (entry->d_type == DT_REG)
            {
                if (strcmp (entry->d_name, "lifespan") != 0)
                {
                    snprintf (counter->filename, sizeof (counter->filename), "%s", entry->d_name);
                    snprintf (counter->pathname, sizeof (counter->pathname), "%s/%s", counters_dir, entry->d_name);

                    total_counters_in_dir++;
                    if (validate_counter_supported (counter))
                    {
                        if (num_counters >= counters_array_size)
                        {
                            counters_array_size += array_grow_size;
                            counters = realloc (counters, counters_array_size * sizeof (counter_sample_t));
                        }

                        counters[num_counters] = *counter;
                        num_counters++;
                        num_supported_counters_in_dir++;
                    }
                }
            }

            entry = readdir (dir);
        }
        closedir (dir);
    }

    if (num_supported_counters_in_dir != total_counters_in_dir)
    {
        printf ("Only %" PRIu32 " of %" PRIu32 " counters in %s are supported\n",
                num_supported_counters_in_dir, total_counters_in_dir, counters_dir);
    }
}


/**
 * @brief Find all statistics counters for the local Infiniband devices
 * @details The hw_counters directory contains RDMA protocol counts:
 *          - With a ConnectX-2 VPI under Ubuntu 18.04 hw_counters appears to be per-device.
 *            This dual-port card enumerates a one device with ports 1 and 2.
 *          - With a ConnectX-4 Lx under AlmaLinux hw_counters is per-port.
 *            This dual-port card enumerates as two devices, each with port 1.
 */
static void find_statistic_counters (void)
{
    int num_ibv_devices = 0;
    struct ibv_device **ibv_device_list;
    struct ibv_context *device_context;
    struct ibv_device_attr device_attributes;
    int device_index;
    int rc;
    char counters_dir[PATH_MAX];

    ibv_device_list = ibv_get_device_list (&num_ibv_devices);
    if (ibv_device_list != NULL)
    {
        for (device_index = 0; device_index < num_ibv_devices; device_index++)
        {
            struct ibv_device *const device = ibv_device_list[device_index];
            counter_sample_t counter = {0};

            device_context = ibv_open_device (device);
            check_assert (device_context != NULL, "ibv_open_device failed");
            rc = ibv_query_device (device_context, &device_attributes);
            check_assert (rc == 0, "ibv_query_device failed");

            /* Get per-device counters */
            snprintf (counter.device_name, sizeof (counter.device_name), "%s", ibv_get_device_name (device));
            counter.per_port = false;
            snprintf (counters_dir, sizeof (counters_dir), "%s/hw_counters", device->ibdev_path);
            add_counters_in_directory (counters_dir, &counter);

            /* Get per-port counters */
            counter.per_port = true;
            for (counter.port_number = 1; counter.port_number <= device_attributes.phys_port_cnt; counter.port_number++)
            {
                snprintf (counters_dir, sizeof (counters_dir), "%s/ports/%" PRIu8 "/counters",
                        device->ibdev_path, counter.port_number);
                add_counters_in_directory (counters_dir, &counter);
                snprintf (counters_dir, sizeof (counters_dir), "%s/ports/%" PRIu8 "/hw_counters",
                        device->ibdev_path, counter.port_number);
                add_counters_in_directory (counters_dir, &counter);
            }

            rc = ibv_close_device (device_context);
            check_assert (rc == 0, "ibv_close_device failed");
        }
    }
}


/**
 * @brief Read all statistics counters for the local Infiniband devices
 */
static void sample_statistics_counters (void)
{
    uint32_t counter_index;
    FILE *counter_file;
    int num_read;

    for (counter_index = 0; counter_index < num_counters; counter_index++)
    {
        counter_sample_t *const counter = &counters[counter_index];

        counter->previous_value = counter->current_value;
        counter_file = fopen (counter->pathname, "r");
        check_assert (counter_file != NULL, "failed to open %s", counter->pathname);
        num_read = fscanf (counter_file, "%" SCNu64, &counter->current_value);
        check_assert (num_read == 1, "failed to read %s", counter->pathname);

        fclose (counter_file);
    }
}


/**
 * @brief Display in tabular form the current values for collected device port counters.
 * @param[in] num_collection_iterations How many collection intervals have occurred, used to display the number of
 *                                      seconds since statistics collection was started.
 */
static void display_collected_statistics (const uint32_t num_collection_iterations)
{
    int counter_name_width = strlen ("Counter name");
    int device_name_width = strlen ("Device");
    int port_width = strlen ("Port");
    uint32_t counter_index;

    for (counter_index = 0; counter_index < num_counters; counter_index++)
    {
        const counter_sample_t *const counter = &counters[counter_index];

        if (strlen (counter->filename) > counter_name_width)
        {
            counter_name_width = strlen (counter->filename);
        }
        if (strlen (counter->device_name) > device_name_width)
        {
            device_name_width = strlen (counter->device_name);
        }
    }

    printf ("\nStatistics after %" PRIu32 " seconds\n", num_collection_iterations * STATISTICS_COLLECTION_PERIOD_SECS);
    printf ("%*s  %*s  %*s  Counter value(delta)\n",
            -counter_name_width, "Counter name",
            -device_name_width, "Device",
            -port_width, "Port");
    for (counter_index = 0; counter_index < num_counters; counter_index++)
    {
        const counter_sample_t *const counter = &counters[counter_index];

        if (counter->current_value > 0)
        {
            printf ("%*s  %*s", -counter_name_width, counter->filename, -device_name_width, counter->device_name);
            if (counter->per_port)
            {
                printf ("  %*" PRIu32, port_width, counter->port_number);
            }
            else
            {
                printf ("  %*s", -port_width, "");
            }

            if (counter->current_value != counter->previous_value)
            {
                printf ("  %" PRIu64 "(+%" PRIu64 ")", counter->current_value, counter->current_value - counter->previous_value);
            }
            else
            {
                printf ("  %" PRIu64, counter->current_value);
            }

            printf ("\n");
        }
    }
}


int main (int argc, char *argv[])
{
    struct sigaction action;
    int rc;
    struct timespec next_sample_time;
    uint32_t num_collection_iterations;

    find_statistic_counters ();
    if (num_counters > 0)
    {
        /* Install a signal handler to allow a request to stop of statistics collection */
        printf ("Press Ctrl-C to stop the Infiniband port statistics collection\n");
        memset (&action, 0, sizeof (action));
        action.sa_handler = stop_statistics_collection_handler;
        action.sa_flags = SA_RESTART;
        rc = sigaction (SIGINT, &action, NULL);
        check_assert (rc == 0, "sigaction");

        sample_statistics_counters ();
        rc = clock_gettime (CLOCK_MONOTONIC, &next_sample_time);
        check_assert (rc == 0, "clock_gettime");

        /* Report Infiniband statistics at regular intervals until requested to stop */
        num_collection_iterations = 0;
        do
        {
            /* Wait until the next collection interval */
            next_sample_time.tv_sec += STATISTICS_COLLECTION_PERIOD_SECS;
            clock_nanosleep (CLOCK_MONOTONIC, TIMER_ABSTIME, &next_sample_time, NULL);
            num_collection_iterations++;

            sample_statistics_counters ();
            display_collected_statistics (num_collection_iterations);
        } while (!exit_requested);
    }
    else
    {
        printf ("No Infiniband devices found\n");
    }

    return EXIT_SUCCESS;
}
