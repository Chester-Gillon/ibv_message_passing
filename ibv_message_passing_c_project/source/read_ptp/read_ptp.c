/*
 * @file read_ptp.c
 * @date 22 July 2018
 * @author Chester Gillon
 * @brief Test access a PTP clock as a POSIX clock.
 * @details This is to look at the overhead in reading a PTP clock, as opposed to trying to check the synchronisation
 *          accuracy of the PTP clock.
 *          For comparison, also measures CLOCK_REALTIME and CLOCK_MONOTONIC in the same way.
 */

#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>

#include <sys/mman.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <phc.h>


/**
 * @brief Calculate the rusage utilisation in seconds between a start and stop time measured by getrusage()
 * @param[in] start_time The start time for the measurement
 * @param[in] stop_time The stop time for the measurement
 * @return Returns the difference between start_time and stop_time in floating point seconds
 */
static double rusage_utilisation_secs (const struct timeval *const start_time, const struct timeval *const stop_time)
{
    const uint64_t us_per_sec = 1000000;
    const uint64_t start_time_us = (start_time->tv_sec * us_per_sec) + start_time->tv_usec;
    const uint64_t stop_time_us = (stop_time->tv_sec * us_per_sec) + stop_time->tv_usec;

    return (double) (stop_time_us - start_time_us) / 1E6;
}


/**
 * @brief Display information for a POSIX clock ID
 * @param[in] id Which POSIX clock ID to display information for
 * @param[in] clock_name The name to display for ID
 */
static void display_clock_info (const clockid_t id, const char *const clock_name)
{
    int rc;
    struct timespec clock_res;
    struct timespec now;


    rc = clock_getres (id, &clock_res);
    if (rc != 0)
    {
        perror ("clock_getres");
        exit (EXIT_FAILURE);
    }
    printf ("%s : res tv_sec=%ld tv_nsec=%ld\n", clock_name, clock_res.tv_sec, clock_res.tv_nsec);

    rc = clock_gettime (id, &now);
    if (rc != 0)
    {
        perror ("clock_getres");
        exit (EXIT_FAILURE);
    }
    printf ("%s : now tv_sec=%ld tv_nsec=%ld\n", clock_name, now.tv_sec, now.tv_nsec);
}


/**
 * @brief Call clock_gettime() in a tight loop to measure the average overhead to obtain a timestamp
 * @param[in] id Which POSIX clock ID to read
 * @param[in] clock_name The name to display for ID
 */
static void time_clock_read (const clockid_t id, const char *const clock_name)
{
    const uint32_t num_iterations = 1000000;
    struct rusage start_usage;
    struct rusage stop_usage;
    int rc;
    uint32_t iteration;
    struct timespec now;

    rc = getrusage (RUSAGE_SELF, &start_usage);
    if (rc != 0)
    {
        perror ("getrusage");
        exit (EXIT_FAILURE);
    }
    for (iteration = 0; iteration < num_iterations; iteration++)
    {
        rc = clock_gettime (id, &now);
        if (rc != 0)
        {
            perror ("clock_gettime");
            exit (EXIT_FAILURE);
        }
    }
    getrusage (RUSAGE_SELF, &stop_usage);
    if (rc != 0)
    {
        perror ("getrusage");
        exit (EXIT_FAILURE);
    }

    printf ("\n%u iterations of clock_gettime(%s) used:\n", num_iterations, clock_name);
    printf ("  minor page faults=%ld (%ld -> %ld)\n",
            stop_usage.ru_minflt - start_usage.ru_minflt,
            start_usage.ru_minflt, stop_usage.ru_minflt);
    printf ("  major page faults=%ld (%ld -> %ld)\n",
            stop_usage.ru_majflt - start_usage.ru_majflt,
            start_usage.ru_majflt, stop_usage.ru_majflt);
    printf ("  voluntary context switches=%ld (%ld -> %ld)\n",
            stop_usage.ru_nvcsw - start_usage.ru_nvcsw,
            start_usage.ru_nvcsw, stop_usage.ru_nvcsw);
    printf ("  involuntary context switches=%ld (%ld -> %ld)\n",
            stop_usage.ru_nivcsw - start_usage.ru_nivcsw,
            start_usage.ru_nivcsw, stop_usage.ru_nivcsw);
    printf ("  user time=%.6f system time=%.6f\n",
            rusage_utilisation_secs (&start_usage.ru_utime, &stop_usage.ru_utime),
            rusage_utilisation_secs (&start_usage.ru_stime, &stop_usage.ru_stime));
}


int main (int argc, char *argv[])
{
    clockid_t ptp_id;
    int rc;

    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s </dev/ptpN>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    if (rc != 0)
    {
        perror ("mlockall");
    }

    char *const ptp_dev_name = argv[1];
    ptp_id = phc_open (ptp_dev_name);
    if (ptp_id == CLOCK_INVALID)
    {
        perror ("phc_open failed");
        exit (EXIT_FAILURE);
    }

    printf ("phc_has_pps=%d\n", phc_has_pps (ptp_id));
    printf ("phc_max_adj=%d\n", phc_max_adj (ptp_id));

    display_clock_info (CLOCK_REALTIME, "CLOCK_REALTIME");
    display_clock_info (CLOCK_MONOTONIC, "CLOCK_MONOTONIC");
    display_clock_info (ptp_id, ptp_dev_name);

    time_clock_read (CLOCK_REALTIME, "CLOCK_REALTIME");
    time_clock_read (CLOCK_MONOTONIC, "CLOCK_MONOTONIC");
    time_clock_read (ptp_id, ptp_dev_name);

    phc_close (ptp_id);

    return EXIT_SUCCESS;
}
