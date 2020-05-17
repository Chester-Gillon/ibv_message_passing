/*
 * @file vector_test_load_main.c
 * @date 27 Aug 2018
 * @author Chester Gillon
 * @brief Program which create threads for generating test loads on CPUs, using either SSE or AVX instructions.
 * @details Each test thread has its affinity set to a a CPU core, and executes vector instructions in a tight loop in a data
 *          set which should fit in the L1 cache.
 *
 *          As the test threads run, at one second intervals they sample the monotonic time and unhalted cycle count, in order
 *          that the CPU core frequency can be measured by the running program.
 *
 *          The Linux perf event is used to configure the unhalted cycle counter to continuously increment for all
 *          processes / threads on the core, which can be sampled from the running user space program with a rdpmc instruction.
 */

#define _GNU_SOURCE

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <immintrin.h>

#include <signal.h>
#include <numa.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>
#include <limits.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include <linux/perf_event.h>


/* The vector test size in bytes, less than the L1D cache size to try and keep the test in the L1 cache */
#define VECTOR_TEST_SIZE_BYTES 30208


/* Size of the stacks for the test threads, dominated by VECTOR_TEST_SIZE_BYTES */
#define TEST_THREAD_STACK_SIZE 65536


/* Set true from a signal handler to request the test threads exit */
static volatile bool test_complete = false;


/* Maximum of CPU frequency samples stored for each test thread */
#define MAX_CPU_FREQ_SAMPLES 10000


/* Contain one sample use to derive a CPU frequency measurement, where adjacent samples are used to calculate the average
 * CPU frequency over the interval. */
typedef struct
{
    /* CLOCK_MONOTONIC at which the sample was taken */
    struct timespec monotonic_time;
    /* The current unhalted cycles count at which the sample was taken */
    uint64_t unhalted_cycles_count;
} cpu_frequency_sample;


/* Contains the context for one test thread */
typedef struct
{
    /* Identity of the thread */
    pthread_t thread_id;
    /* Stack of the thread, allocated on the local NUMA node for core */
    void *thread_stack;
    /* Which core the thread is pinned to */
    int core;
    /* When true AVX instructions on 256-bit vectors are performed.
     * When false SSE instructions on 128-bit vectors are performed. */
    bool avx_load_thread;
    /* The total number of test iterations performed */
    uint64_t num_test_iterations;
    /* Used to store the final sum from the test threads, which is displayed.
     * This gives a visible side effect for the vector operations, to prevent the compiler optimiser from removing them. */
    float final_sum[sizeof (__m256) / sizeof (float)];
    /* The perf file descriptor for the unhalted cycle counter */
    int cycle_counter_fd;
    /* The perf mmaped page for cycle_counter_fd, used to obtain the index to the counter to be able to use a RDPMC instruction to
     * read the counter directly from user space. */
    struct perf_event_mmap_page *cycle_counter_mmap_page;
    /* The CLOCK_MONOTONIC time to obtain the next CPU frequency sample.
     * The main thread sets the same value on all threads, to get the threads to sample at the same time.
     * All the threads advance by the same amount, to maintain synchronised sampling. */
    struct timespec time_for_next_cpu_frequency_sample;
    /* The array of CPU frequency samples taken while the test thread is running */
    uint64_t num_cpu_frequency_samples;
    cpu_frequency_sample cpu_frequency_samples[MAX_CPU_FREQ_SAMPLES];
    /* If true the test is ended when the current time reached test_end_time */
    bool test_end_time_valid;
    /* The CLOCK_MONOTONIC time at which the test ends */
    struct timespec test_end_time;
    /* Set once the current time exceeds test_end_time */
    bool test_end_time_exceeded;
} test_thread_context;


/**
 * @brief Abort the program if a Linux function indicates an error status
 * @param[in] rc Zero on success
 * @param[in] message Displayed if an error has occurred.
 */
static void check_linux_status (const int rc, const char *message)
{
    if (rc != 0)
    {
        perror (message);
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Signal handler to request stopping the test
 */
static void stop_test_handler (const int sig)
{
    test_complete = true;
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
 * @brief Read the current value of a performance counter
 * @param[in] counter Which counter to read
 * @return The current value, as a 64-bit value. The actual hardware counter width may be smaller.
 */
static inline uint64_t rdpmc(const uint32_t counter)
{
    uint32_t low, high;
    __asm__ __volatile__("rdpmc" : "=a" (low), "=d" (high) : "c" (counter));
    return (uint64_t)high << 32 | (uint64_t)low;
}


/**
 * @brief Initialise the core frequency measurement for one test thread
 * @details The thread is assumed to have it affinity set to only the core given in the context, as the performance counter used
 *          to sample the core frequency is set to only used on the core for the thread.
 * @param[in,out] context The thread context to initialise the core frequency measurement for
 */
static void initialise_core_frequency_measurement (test_thread_context *const context)
{
    const size_t page_size = (size_t) getpagesize ();
    struct perf_event_attr attr =
    {
        .type = PERF_TYPE_HARDWARE,
        .config = PERF_COUNT_HW_CPU_CYCLES
    };

    /* Open perf event to measure unhalted cycles on all processes/threads on the CPU the test thread runs on.
     * By specifying measurement on all processes/threads, the unhalted cycle count should still increment even if the calling
     * thread has involuntary context switches. */
    errno = 0;
    context->cycle_counter_fd = syscall (__NR_perf_event_open, &attr, -1, context->core, -1, 0);
    if (context->cycle_counter_fd == -1)
    {
        if (errno == EACCES)
        {
            fprintf (stderr, "No permission to open perf event for all processes/threads.\n"
                             "This requires CAP_SYS_ADMIN capability or a /proc/sys/kernel/perf_event_paranoid value of less than 1.\n");

        }
        perror ("__NR_perf_event_open");
        exit (EXIT_FAILURE);
    }

    /* Map a single page for the perf event just to get the index for the performance counter. */
    context->cycle_counter_mmap_page = mmap (NULL, page_size, PROT_READ, MAP_SHARED, context->cycle_counter_fd, 0);
    if (context->cycle_counter_mmap_page == MAP_FAILED)
    {
        perror ("mmap");
        exit (EXIT_FAILURE);
    }

#ifdef HAVE_PERF_CAP_USER_RDPMC
    if (!context->cycle_counter_mmap_page->cap_user_rdpmc)
    {
        fprintf (stderr, "No support for cap_user_rdpmc\n");
        exit (EXIT_FAILURE);
    }
#endif
    if (context->cycle_counter_mmap_page->index == 0)
    {
        fprintf (stderr, "No performance counter index\n");
        exit (EXIT_FAILURE);
    }

    context->num_cpu_frequency_samples = 0;
    context->test_end_time_exceeded = false;
}


/**
 * @brief Shutdown the core frequency measurement for one test thread
 * @param[in,out] The thread context to shutdown the core frequency measurement for
 */
static void shutdown_core_frequency_measurement (test_thread_context *const context)
{
    const size_t page_size = (size_t) getpagesize ();
    int rc;

    rc = munmap (context->cycle_counter_mmap_page, page_size);
    check_linux_status (rc, "munmap");

    close (context->cycle_counter_fd);
}


/**
 * @brief Determine if an interval timer has expired
 * @param[in] now Current time
 * @param[in] end_time Interval time expiry time
 * @return Returns when now >= end_time
 */
static bool interval_time_exceeded (const struct timespec *const now, const struct timespec *const end_time)
{
    return ( (now->tv_sec > end_time->tv_sec                                         ) ||
            ((now->tv_sec == end_time->tv_sec) && (now->tv_nsec >= end_time->tv_nsec))   );
}


/**
 * @details Called after each iteration in a test thread to:
 *          a) Store a sample for the core CPU frequency measurement, when time
 *          b) Determine if the test duration has expired
 * @param[in,out] context The thread context
 */
static void schedule_cpu_frequency_sample (test_thread_context *const context)
{
    int rc;
    struct timespec time_now;

    rc = clock_gettime (CLOCK_MONOTONIC, &time_now);
    check_linux_status (rc, "clock_gettime");

    if (context->num_cpu_frequency_samples < MAX_CPU_FREQ_SAMPLES)
    {
        if (interval_time_exceeded (&time_now, &context->time_for_next_cpu_frequency_sample))
        {
            cpu_frequency_sample *const sample = &context->cpu_frequency_samples[context->num_cpu_frequency_samples];
            int64_t pmc;
#ifdef HAVE_PERF_PMC_WIDTH
            const uint16_t pmc_sign_extend_shift = 64 - context->cycle_counter_mmap_page->pmc_width;
#else
            /* Have to assume standard Intel 48-bit PMC counters if the Kernel perf versiom doesn't report
             * the actual width.
             * Should be valid for Sandy Bridge and Haswell processors. */
            const uint16_t assumed_pmc_width = 48;
            const uint16_t pmc_sign_extend_shift = 64 - assumed_pmc_width;
#endif

            rc = clock_gettime (CLOCK_MONOTONIC, &sample->monotonic_time);
            pmc = (int64_t) rdpmc (context->cycle_counter_mmap_page->index - 1u);
            check_linux_status (rc, "clock_gettime");

            /* Build the 64-bit count by adding the sign-extended PMC count onto the offset maintained by perf.
             * This results in the 64-bit unhalted_cycles_count incrementing monotonically when the lower width PMC count wraps.
             * (perf initialises the PMC counter with its most-significant bit set and the offset as non-zero). */
            pmc <<= pmc_sign_extend_shift;
            pmc >>= pmc_sign_extend_shift;
            sample->unhalted_cycles_count = (uint64_t) (context->cycle_counter_mmap_page->offset + pmc);

            context->num_cpu_frequency_samples++;
            context->time_for_next_cpu_frequency_sample.tv_sec++;
        }
    }

    if (context->test_end_time_valid && interval_time_exceeded (&time_now, &context->test_end_time))
    {
        context->test_end_time_exceeded = true;
    }
}

/**
 * @brief Thread which generates a CPU test load by executing AVX instruction on 256-bit vectors in a tight loop.
 * @param[in,out] arg The context for the thread
 * @return Not used
 */
static void *avx_test_load_thread (void *const arg)
{
    test_thread_context *const context = arg;
    const size_t num_vectors = VECTOR_TEST_SIZE_BYTES / sizeof (__m256);
    __m256 vectors_to_sum[num_vectors];
    __m256 sum;
    int vector_index;
    uint64_t num_test_iterations;

    /* Initialise the vector to sum with a deterministic pattern */
    for (vector_index = 0; vector_index < num_vectors; vector_index++)
    {
        const float vector_value[] =
        {
            1.0 + (0.1 *vector_index), 2.0 + (0.1 *vector_index), 3.0 + (0.1 *vector_index), 4.0 + (0.1 *vector_index),
            5.0 + (0.1 *vector_index), 6.0 + (0.1 *vector_index), 7.0 + (0.1 *vector_index), 8.0 + (0.1 *vector_index)
        };

        vectors_to_sum[vector_index] = _mm256_load_ps (vector_value);
    }

    /* Sum the test vector until told to stop */
    initialise_core_frequency_measurement (context);
    num_test_iterations = 0;
    while (!test_complete && !context->test_end_time_exceeded)
    {
        sum = _mm256_setzero_ps ();
        for (vector_index = 0; vector_index < num_vectors; vector_index += 8)
        {
            sum = _mm256_fmadd_ps (
                _mm256_fmadd_ps (_mm256_fmadd_ps (vectors_to_sum[vector_index+1], vectors_to_sum[vector_index+2], vectors_to_sum[vector_index+3]),
                                 _mm256_fmadd_ps (vectors_to_sum[vector_index+2], vectors_to_sum[vector_index+3], vectors_to_sum[vector_index+4]),
                                                  vectors_to_sum[vector_index+5]),
                _mm256_fmadd_ps (_mm256_fmadd_ps (vectors_to_sum[vector_index+3], vectors_to_sum[vector_index+4], vectors_to_sum[vector_index+5]),
                                 _mm256_fmadd_ps (vectors_to_sum[vector_index+4], vectors_to_sum[vector_index+5], vectors_to_sum[vector_index+6]),
                                                  vectors_to_sum[vector_index+7]),
                _mm256_fmadd_ps (_mm256_fmadd_ps (vectors_to_sum[vector_index+3], vectors_to_sum[vector_index+2], vectors_to_sum[vector_index+1]),
                                 _mm256_fmadd_ps (vectors_to_sum[vector_index+4], vectors_to_sum[vector_index+5], vectors_to_sum[vector_index+3]),
                                                  vectors_to_sum[vector_index+5]));
        }
        num_test_iterations++;
        schedule_cpu_frequency_sample (context);
    }

    shutdown_core_frequency_measurement (context);
    context->num_test_iterations = num_test_iterations;
    _mm256_storeu_ps (context->final_sum, sum);

    return NULL;
}


/**
 * @brief Thread which generates a CPU test load by executing SSE instruction on 128-bit vectors in a tight loop.
 * @param[in,out] arg The context for the thread
 * @return Not used
 */
static void *sse_test_load_thread (void *const arg)
{
    test_thread_context *const context = arg;
    const size_t num_vectors = VECTOR_TEST_SIZE_BYTES / sizeof (__m128);
    __m128 vectors_to_sum[num_vectors];
    __m128 sum;
    int vector_index;
    uint64_t num_test_iterations;

    /* Initialise the vector to sum with a deterministic pattern */
    for (vector_index = 0; vector_index < num_vectors; vector_index++)
    {
        const float vector_value[] =
        {
            1.0 + (0.1 *vector_index), 2.0 + (0.1 *vector_index), 3.0 + (0.1 *vector_index), 4.0 + (0.1 *vector_index)
        };

        vectors_to_sum[vector_index] = _mm_load_ps (vector_value);
    }

    /* Sum the test vector until told to stop */
    initialise_core_frequency_measurement (context);
    num_test_iterations = 0;
    while (!test_complete && !context->test_end_time_exceeded)
    {
        sum = _mm_setzero_ps ();
        for (vector_index = 0; vector_index < num_vectors; vector_index++)
        {
            sum = _mm_add_ps (sum, vectors_to_sum[vector_index]);
        }
        num_test_iterations++;
        schedule_cpu_frequency_sample (context);
    }

    shutdown_core_frequency_measurement (context);
    context->num_test_iterations = num_test_iterations;
    _mm_storeu_ps (context->final_sum, sum);

    return NULL;
}


/**
 * @brief Write a CSV file which contains the results of the CPU frequency samples
 * @param[in] basename The basename component for the CSV file
 * @param[in] avx_cores_string The string with the AVX cores, added to the filename.
 * @param[in] sse_cores_string The string with the SSE cores, added to the filename.
 * @param[in] num_threads The number of threads to write the results for.
 * @param[in] contexts The contexts for the threads to write the results for.
 */
static void write_core_frequency_csv_file (const char *const results_basename,
                                           const char *const avx_cores_string, const char *const sse_cores_string,
                                           const int num_threads, test_thread_context *contexts[const num_threads])
{
    char results_pathname[PATH_MAX];
    FILE *results_file;
    size_t path_len;
    int thread_index;

    /* Build CSV pathname and create file */
    snprintf (results_pathname, sizeof (results_pathname), "%s", results_basename);
    if (avx_cores_string != NULL)
    {
        path_len = strlen (results_pathname);
        snprintf (&results_pathname[path_len], sizeof (results_pathname) - path_len, "_AVX%s", avx_cores_string);
    }
    if (sse_cores_string != NULL)
    {
        path_len = strlen (results_pathname);
        snprintf (&results_pathname[path_len], sizeof (results_pathname) - path_len, "_SSE%s", sse_cores_string);
    }
    path_len = strlen (results_pathname);
    snprintf (&results_pathname[path_len], sizeof (results_pathname) - path_len, ".csv");
    results_file = fopen (results_pathname, "w");
    if (results_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", results_pathname);
        exit (EXIT_FAILURE);
    }

    /* Write column headers */
    fprintf (results_file, ",");
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        const test_thread_context *const context = contexts[thread_index];

        fprintf (results_file, "%s core %d,", context->avx_load_thread ? "AVX" : "SSE", context->core);
    }
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        const test_thread_context *const context = contexts[thread_index];

        fprintf (results_file, "%s core %d,", context->avx_load_thread ? "AVX" : "SSE", context->core);
        fprintf (results_file, "%s core %d,", context->avx_load_thread ? "AVX" : "SSE", context->core);
    }
    fprintf (results_file, "\n");

    fprintf (results_file, ",");
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        fprintf (results_file, "Core freq (GHz),");
    }
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        fprintf (results_file, "Monotonic time (secs),");
        fprintf (results_file, "Unhalted cycles,");
    }
    fprintf (results_file, "\n");

    /* Find the maximum number of samples across all threads */
    uint64_t max_samples = 0u;
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        const test_thread_context *const context = contexts[thread_index];

        if (context->num_cpu_frequency_samples > max_samples)
        {
            max_samples = context->num_cpu_frequency_samples;
        }
    }

    /* Report the core frequency for all samples */
    for (uint64_t sample_index = 0; (sample_index + 1u) < max_samples; sample_index++)
    {
        fprintf (results_file, "Sample %" PRIu64 ",", sample_index + 1);
        for (thread_index = 0; thread_index < num_threads; thread_index++)
        {
            const test_thread_context *const context = contexts[thread_index];

            if ((sample_index > 0u) && (sample_index < context->num_cpu_frequency_samples))
            {
                /* Derive the core frequency over one sample period */
                const cpu_frequency_sample *const previous = &context->cpu_frequency_samples[sample_index - 1u];
                const cpu_frequency_sample *const current = &context->cpu_frequency_samples[sample_index];
                const int64_t elapsed_ns = get_elapsed_ns (&previous->monotonic_time, &current->monotonic_time);
                const uint64_t elapsed_unhalted_cycles = current->unhalted_cycles_count - previous->unhalted_cycles_count;
                const double core_frequency_ghz = (double) elapsed_unhalted_cycles / (double) elapsed_ns;

                fprintf (results_file, "%.3f,", core_frequency_ghz);
            }
            else
            {
                fprintf (results_file, ",");
            }
        }

        /* Display the raw samples */
        for (thread_index = 0; thread_index < num_threads; thread_index++)
        {
            const test_thread_context *const context = contexts[thread_index];

            if (sample_index < context->num_cpu_frequency_samples)
            {
                const cpu_frequency_sample *const current = &context->cpu_frequency_samples[sample_index];

                fprintf (results_file, "%ld.%09ld,%" PRIu64 ",",
                        current->monotonic_time.tv_sec, current->monotonic_time.tv_nsec, current->unhalted_cycles_count);
            }
            else
            {
                fprintf (results_file, ",");
            }
        }
        fprintf (results_file, "\n");
    }

    /* Report the average core frequency over all samples */
    fprintf (results_file, "Average,");
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        const test_thread_context *const context = contexts[thread_index];

        if (context->num_cpu_frequency_samples >= 1u)
        {
            const cpu_frequency_sample *const first = &context->cpu_frequency_samples[0];
            const cpu_frequency_sample *const last = &context->cpu_frequency_samples[context->num_cpu_frequency_samples - 1u];
            const int64_t elapsed_ns = get_elapsed_ns (&first->monotonic_time, &last->monotonic_time);
            const uint64_t elapsed_unhalted_cycles = last->unhalted_cycles_count - first->unhalted_cycles_count;
            const double core_frequency_ghz = (double) elapsed_unhalted_cycles / (double) elapsed_ns;

            fprintf (results_file, "%.3f,", core_frequency_ghz);
        }
    }
    fprintf (results_file, "\n");

    fclose (results_file);
}

int main (int argc, char *argv[])
{
    int arg_index;
    int rc;
    struct timespec now;
    const char *avx_cores_string = NULL;
    const char *sse_cores_string = NULL;
    struct bitmask *avx_cores_mask = NULL;
    struct bitmask *sse_cores_mask = NULL;
    bool test_duration_present = false;
    int test_duration_secs = 0;
    const char *results_basename = NULL;
    char junk;

    if (argc == 1)
    {
        fprintf (stderr, "Usage: %s [-a <avx_cores_mask>] [-s <sse_cores_mask>] [-d <test_duration_secs>] [-o <results_basename>]\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    rc = numa_available ();
    if (rc == -1)
    {
        fprintf (stderr, "NUMA is not available\n");
        exit (EXIT_FAILURE);
    }

    /* Extract the AVX and/or SSE core masks from the command line arguments */
    arg_index = 1;
    while (arg_index < argc)
    {
        if (strcmp (argv[arg_index], "-a") == 0)
        {
            avx_cores_string = argv[arg_index + 1];
            avx_cores_mask = numa_parse_cpustring (avx_cores_string);
            if (avx_cores_mask == NULL)
            {
                fprintf (stderr, "AVX cores mask invalid\n");
                exit (EXIT_FAILURE);
            }
            arg_index += 2;
        }
        else if (strcmp (argv[arg_index], "-s") == 0)
        {
            sse_cores_string = argv[arg_index + 1];
            sse_cores_mask = numa_parse_cpustring (sse_cores_string);
            if (sse_cores_mask == NULL)
            {
                fprintf (stderr, "SSE cores mask invalid\n");
                exit (EXIT_FAILURE);
            }
            arg_index += 2;
        }
        else if (strcmp (argv[arg_index], "-d") == 0)
        {
            if ((sscanf (argv[arg_index + 1], "%d%c", &test_duration_secs, &junk) != 1) || (test_duration_secs <= 0))
            {
                fprintf (stderr, "Invalid test duration\n");
                exit (EXIT_FAILURE);
            }
            test_duration_present = true;
            arg_index += 2;
        }
        else if (strcmp (argv[arg_index], "-o") == 0)
        {
            results_basename = argv[arg_index + 1];
            arg_index += 2;
        }
        else
        {
            fprintf (stderr, "Unknown argument %s\n", argv[arg_index]);
            exit (EXIT_FAILURE);
        }
    }

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    check_linux_status (rc, "mlockall");

    /* Create an array of the test threads to create */
    const int num_cpus = numa_num_configured_cpus ();
    test_thread_context **const contexts = calloc (num_cpus, sizeof (test_thread_context *));
    int num_threads = 0;
    int core;
    if (avx_cores_mask != NULL)
    {
        for (core = 0; core < num_cpus; core++)
        {
            if (numa_bitmask_isbitset (avx_cores_mask, core))
            {
                const int numa_node = numa_node_of_cpu (core);
                test_thread_context *const context = numa_alloc_onnode (sizeof (test_thread_context), numa_node);

                contexts[num_threads] = context;
                memset (context, 0, sizeof (test_thread_context));
                context->num_test_iterations = 0;
                context->core = core;
                context->avx_load_thread = true;
                num_threads++;
            }
        }
    }

    if (sse_cores_mask != NULL)
    {
        for (core = 0; core < num_cpus; core++)
        {
            if (numa_bitmask_isbitset (sse_cores_mask, core))
            {
                const int numa_node = numa_node_of_cpu (core);
                test_thread_context *const context = numa_alloc_onnode (sizeof (test_thread_context), numa_node);

                contexts[num_threads] = context;
                memset (context, 0, sizeof (test_thread_context));
                context->num_test_iterations = 0;
                context->core = core;
                context->avx_load_thread = false;
                num_threads++;
            }
        }
    }

    /* Get all threads to start their CPU frequency measurement one second into the future,
     * and optionally set the test end time */
    int thread_index;
    struct timespec time_for_next_cpu_frequency_sample;
    rc = clock_gettime (CLOCK_MONOTONIC, &now);
    check_linux_status (rc, "clock_gettime");
    time_for_next_cpu_frequency_sample = now;
    time_for_next_cpu_frequency_sample.tv_sec++;
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        test_thread_context *const context = contexts[thread_index];

        context->time_for_next_cpu_frequency_sample = time_for_next_cpu_frequency_sample;
        context->test_end_time_valid = test_duration_present;
        context->test_end_time = time_for_next_cpu_frequency_sample;
        context->test_end_time.tv_sec += test_duration_secs;
    }

    /* Create the test threads, with their CPU affinity set to the specified core and with their stack allocated from the
     *  local NUMA node.
     *  @todo doesn't set a guard for the allocated stack. */
    pthread_attr_t thread_attr;
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        test_thread_context *const context = contexts[thread_index];
        cpu_set_t cpuset;
        const int numa_node = numa_node_of_cpu (context->core);

        rc = pthread_attr_init (&thread_attr);
        check_linux_status (rc, "pthread_attr_init");

        context->thread_stack = numa_alloc_onnode (TEST_THREAD_STACK_SIZE, numa_node);
        if (context->thread_stack == NULL)
        {
            check_linux_status (-1, "numa_alloc_on_node");
        }
        rc = pthread_attr_setstack (&thread_attr, context->thread_stack, TEST_THREAD_STACK_SIZE);
        check_linux_status (rc, "pthread_attr_setstack");

        CPU_ZERO (&cpuset);
        CPU_SET (context->core, &cpuset);
        rc = pthread_attr_setaffinity_np (&thread_attr, sizeof (cpuset), &cpuset);
        check_linux_status (rc, "pthread_attr_setaffinty_np");

        rc = pthread_create (&context->thread_id, &thread_attr,
                context->avx_load_thread ? avx_test_load_thread : sse_test_load_thread, context);
        check_linux_status (rc, "pthread_create");

        rc = pthread_attr_destroy (&thread_attr);
        check_linux_status (rc, "pthread_attr_destroy");
    }

    /* Install a signal handler to allow a request to stop the test */
    struct sigaction action;

    printf ("Press Ctrl-C to tell the %u thread(s) to stop the test\n", num_threads);
    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_test_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    check_linux_status (rc, "sigaction");

    /* Wait for the threads to exit */
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        test_thread_context *const context = contexts[thread_index];
        void *thread_return;

        rc = pthread_join (context->thread_id, &thread_return);
        check_linux_status (rc, "pthread_join");
    }

    if (results_basename != NULL)
    {
        write_core_frequency_csv_file (results_basename, avx_cores_string, sse_cores_string, num_threads, contexts);
    }

    /* Display the statistics on the number of iterations performed by each thread */
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        const test_thread_context *const context = contexts[thread_index];

        printf ("%s core %d num_test_iterations = %" PRIu64 " (final sum[0]=%.6g)\n",
                context->avx_load_thread ? "AVX" : "SSE", context->core, context->num_test_iterations, context->final_sum[0]);
    }

    return EXIT_SUCCESS;
}
