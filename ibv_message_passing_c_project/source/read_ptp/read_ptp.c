/*
 * @file read_ptp.c
 * @date 22 July 2018
 * @author Chester Gillon
 * @brief Test access a PTP clock as a POSIX clock.
 * @details This is to look at the overhead in reading a PTP clock, as opposed to trying to check the synchronisation
 *          accuracy of the PTP clock.
 *          For comparison, also measures CLOCK_REALTIME and CLOCK_MONOTONIC in the same way.
 */

#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>

#include <sys/mman.h>
#include <sys/time.h>
#include <sys/resource.h>

/* Build an include file name from a Macro, to test the CMake dependency generation */
#define PTP_INCLUDE_STRINGIFY(FILE) #FILE
#define PTP_INCLUDE(PTP_MAPPING) PTP_INCLUDE_STRINGIFY(PTP_MAPPING.h)
#include PTP_INCLUDE(PTP_MAPPING)

#include <linux/perf_event.h>


/**
 * @brief Call clock_getime(), checking the result
 * @param[in] id Which clock to read
 * @param[out] now The current time
 */
static void checked_gettime (const clockid_t id, struct timespec *const now)
{
    int rc;

    rc = clock_gettime (id, now);
    if (rc != 0)
    {
        perror ("clock_gettime");
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Return the elapsed time in nanoseconds between two time stamps
 * @param[in] start_time The start time for the elapsed duration
 * @param[in] end_time The end time for the elapsed duration
 * @return Returns the elapsed time in nanoseconds
 */
static uint64_t get_elapsed_ns (const struct timespec *const start_time, const struct timespec *const end_time)
{
    const uint64_t nsecs_per_sec = 1000000000;
    const uint64_t start_time_ns = (start_time->tv_sec * nsecs_per_sec) + start_time->tv_nsec;
    const uint64_t end_time_ns = (end_time->tv_sec * nsecs_per_sec) + end_time->tv_nsec;

    return end_time_ns - start_time_ns;
}


/**
 * @brief Open a file descriptor to read the hardware CPU counter from perf events
 * @return The file descriptor to read the perf event
 */
static int open_cycle_counter_fd (void)
{
    int fd;
    struct perf_event_attr attr =
    {
        .type = PERF_TYPE_HARDWARE,
        .config = PERF_COUNT_HW_CPU_CYCLES
    };

    fd = syscall (__NR_perf_event_open, &attr, 0, -1, -1, 0);
    if (fd == -1)
    {
        perror ("__NR_perf_event_open");
        exit (EXIT_FAILURE);
    }

    return fd;
}


/**
 * @brief Read the current value of the cycle counter
 * @param[in] cycle_counter_fd The file descriptor returned by open_cycle_counter_fd()
 * @return The current cycle counter value
 */
static uint64_t get_cycle_counter (const int cycle_counter_fd)
{
    uint64_t count;
    ssize_t bytes_read;

    bytes_read = read (cycle_counter_fd, &count, sizeof (count));
    if (bytes_read < sizeof (count))
    {
        perror ("read");
        exit (EXIT_FAILURE);
    }

    return count;
}


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

    checked_gettime (id, &now);
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
        checked_gettime (id, &now);
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


/**
 * @brief Display an estimate of the CPU frequency
 * @details This is done by measuring the change in the hardware CPU counter perf event while waiting for one
 *          second to elapse while reading a POSIX clock.
 *          It assumes that frequency scaling doesn't the CPU frequency while this function is running.
 * @param[in] id Which POSIX clock ID to read
 * @param[in] clock_name The name to display for ID
 * @param[in] cycle_counter_fd The file descriptor returned by open_cycle_counter_fd()
 */
static void estimate_cpu_frequency (const clockid_t id, const char *const clock_name, const int cycle_counter_fd)
{
    struct timespec monotonic_start_time;
    struct timespec monotonic_end_time;
    struct timespec monotonic_raw_start_time;
    struct timespec monotonic_raw_end_time;
    struct timespec realtime_start_time;
    struct timespec realtime_end_time;
    struct timespec start_time;
    struct timespec end_time;
    struct timespec now;
    uint64_t start_counter;
    uint64_t end_counter;

    checked_gettime (CLOCK_REALTIME, &realtime_start_time);
    checked_gettime (CLOCK_MONOTONIC, &monotonic_start_time);
    checked_gettime (CLOCK_MONOTONIC_RAW, &monotonic_raw_start_time);
    checked_gettime (id, &start_time);
    start_counter = get_cycle_counter (cycle_counter_fd);
    end_time = start_time;
    end_time.tv_sec++;
    do
    {
        checked_gettime (id, &now);
    } while ((now.tv_sec < end_time.tv_sec) ||
             ((now.tv_sec == end_time.tv_sec) && (now.tv_nsec < end_time.tv_nsec)));
    end_counter = get_cycle_counter (cycle_counter_fd);
    checked_gettime (CLOCK_MONOTONIC_RAW, &monotonic_raw_end_time);
    checked_gettime (CLOCK_MONOTONIC, &monotonic_end_time);
    checked_gettime (CLOCK_REALTIME, &realtime_end_time);

    printf ("One second of elapsed time using %s took %" PRIu64 " HW CPU cycles (%" PRIu64 " -> %" PRIu64 ")\n",
            clock_name, end_counter - start_counter, start_counter, end_counter);

    const uint64_t elapsed_realtime = get_elapsed_ns (&realtime_start_time, &realtime_end_time);
    const uint64_t elapsed_monotonic = get_elapsed_ns (&monotonic_start_time, &monotonic_end_time);
    const uint64_t elapsed_raw_monotonic = get_elapsed_ns (&monotonic_raw_start_time, &monotonic_raw_end_time);

    printf ("  while CLOCK_MONOTONIC_RAW advanced %" PRIu64 " nsecs, CLOCK_MONOTONIC advanced %" PRIu64 " nsecs and CLOCKREAL_TIME advanced %" PRIu64 " nsecs\n",
            elapsed_raw_monotonic, elapsed_monotonic, elapsed_realtime);
}


/**
 * @brief Measure the rate of two different clocks, by comparing the rate of the "other" clock to the "reference" clock
 * @param[in] ref_id The reference clock
 * @param[in] ref_clock_name Used to identify ref_id
 * @param[in] other_id The clock to compare to the reference
 * @param[in] other_clock_name Used to identify other_id
 */
static void compare_clock_rates (const clockid_t ref_id, const char *const ref_clock_name,
                                 const clockid_t other_id, const char *const other_clock_name)
{
    struct timespec other_start_time;
    struct timespec other_end_time;
    struct timespec ref_start_time;
    struct timespec ref_end_time;
    struct timespec ref_now;
    double ref_elapsed_ns;
    double other_elapsed_ns;

    checked_gettime (ref_id, &ref_start_time);
    checked_gettime (other_id, &other_start_time);
    ref_end_time = ref_start_time;
    ref_end_time.tv_sec++;
    do
    {
        checked_gettime (ref_id, &ref_now);
    } while ((ref_now.tv_sec < ref_end_time.tv_sec) ||
             ((ref_now.tv_sec == ref_end_time.tv_sec) && (ref_now.tv_nsec < ref_end_time.tv_nsec)));
    checked_gettime (other_id, &other_end_time);

    ref_elapsed_ns = get_elapsed_ns (&ref_start_time, &ref_now);
    other_elapsed_ns = get_elapsed_ns (&other_start_time, &other_end_time);
    printf ("While reference %s advanced %.9f seconds, %s advanced %.9f seconds (or %.1f ppm difference)\n",
            ref_clock_name, ref_elapsed_ns / 1E9, other_clock_name, other_elapsed_ns / 1E9,
            ((other_elapsed_ns / ref_elapsed_ns) - 1.0) * 1E6);
}


int main (int argc, char *argv[])
{
    clockid_t ptp_id;
    int cycle_counter_fd;
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

    cycle_counter_fd = open_cycle_counter_fd ();

    display_clock_info (CLOCK_REALTIME, "CLOCK_REALTIME");
    display_clock_info (CLOCK_MONOTONIC, "CLOCK_MONOTONIC");
    display_clock_info (CLOCK_MONOTONIC_RAW, "CLOCK_MONOTONIC_RAW");
    display_clock_info (ptp_id, ptp_dev_name);

    time_clock_read (CLOCK_REALTIME, "CLOCK_REALTIME");
    estimate_cpu_frequency (CLOCK_REALTIME, "CLOCK_REALTIME", cycle_counter_fd);
    time_clock_read (CLOCK_MONOTONIC, "CLOCK_MONOTONIC");
    estimate_cpu_frequency (CLOCK_MONOTONIC, "CLOCK_MONOTONIC", cycle_counter_fd);
    time_clock_read (CLOCK_MONOTONIC_RAW, "CLOCK_MONOTONIC_RAW");
    estimate_cpu_frequency (CLOCK_MONOTONIC_RAW, "CLOCK_MONOTONIC_RAW", cycle_counter_fd);
    time_clock_read (ptp_id, ptp_dev_name);
    estimate_cpu_frequency (ptp_id, ptp_dev_name, cycle_counter_fd);

    compare_clock_rates (CLOCK_REALTIME, "CLOCK_REALTIME", CLOCK_MONOTONIC, "CLOCK_MONOTONIC");
    compare_clock_rates (CLOCK_REALTIME, "CLOCK_REALTIME", CLOCK_MONOTONIC_RAW, "CLOCK_MONOTONIC_RAW");
    compare_clock_rates (CLOCK_REALTIME, "CLOCK_REALTIME", ptp_id, ptp_dev_name);

    phc_close (ptp_id);
    close (cycle_counter_fd);

    return EXIT_SUCCESS;
}
