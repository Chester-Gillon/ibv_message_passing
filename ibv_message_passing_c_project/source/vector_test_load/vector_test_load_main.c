/*
 * @file vector_test_load_main.c
 * @date 27 Aug 2018
 * @author Chester Gillon
 * @brief Program which create threads for generating test loads on CPUs, using either SSE or AVX instructions.
 * @details Each test thread has its affinity set to a a CPU core, and executes vector instructions in a tight loop in a data
 *          set which should fit in the L1 cache.
 * @todo    The intent was to determine if running cores with AVX instructions on a Haswell/EP caused the frequency reported by
 *          https://github.com/cyring/CoreFreq to drop below the non-AVX base frequency.
 *          So far, have not seen the effect. The avx_test_load_thread() is simplistic in that doesn't attempt to run maximise
 *          use of the AVX pipeline.
 */

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <immintrin.h>

#include <signal.h>
#include <numa.h>
#include <pthread.h>


/* The vector test size in bytes, less than the L1D cache size to try and keep the test in the L1 cache */
#define VECTOR_TEST_SIZE_BYTES 30016


/* Size of the stacks for the test threads, dominated by VECTOR_TEST_SIZE_BYTES */
#define TEST_THREAD_STACK_SIZE 65536


/* Set true from a signal handler to request the test threads exit */
static volatile bool test_complete = false;


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
 * @brief Signal handler to request the test is stopped
 */
static void stop_test_handler (const int sig)
{
    test_complete = true;
}

/**
 * @brief Thread which generates a CPU test load by executing AVX instruction on 256-bit vectors in a tight loop.
 * @param[in/out] arg The context for the thread
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
    num_test_iterations = 0;
    while (!test_complete)
    {
        sum = _mm256_setzero_ps ();
        for (vector_index = 0; vector_index < num_vectors; vector_index++)
        {
            sum = _mm256_add_ps (sum, vectors_to_sum[vector_index]);
        }
        num_test_iterations++;
    }

    context->num_test_iterations = num_test_iterations;
    _mm256_storeu_ps (context->final_sum, sum);

    return NULL;
}


/**
 * @brief Thread which generates a CPU test load by executing SSE instruction on 128-bit vectors in a tight loop.
 * @param[in/out] arg The context for the thread
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
    num_test_iterations = 0;
    while (!test_complete)
    {
        sum = _mm_setzero_ps ();
        for (vector_index = 0; vector_index < num_vectors; vector_index++)
        {
            sum = _mm_add_ps (sum, vectors_to_sum[vector_index]);
        }
        num_test_iterations++;
    }

    context->num_test_iterations = num_test_iterations;
    _mm_storeu_ps (context->final_sum, sum);

    return NULL;
}

int main (int argc, char *argv[])
{
    int arg_index;
    int rc;
    struct bitmask *avx_cores_mask = NULL;
    struct bitmask *sse_cores_mask = NULL;

    if (argc == 1)
    {
        fprintf (stderr, "Usage: %s [-a <avx_cores_mask>] [-s <sse_cores_mask>]\n", argv[0]);
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
            avx_cores_mask = numa_parse_cpustring (argv[arg_index + 1]);
            if (avx_cores_mask == NULL)
            {
                fprintf (stderr, "AVX cores mask invalid\n");
                exit (EXIT_FAILURE);
            }
            arg_index += 2;
        }
        else if (strcmp (argv[arg_index], "-s") == 0)
        {
            sse_cores_mask = numa_parse_cpustring (argv[arg_index + 1]);
            if (sse_cores_mask == NULL)
            {
                fprintf (stderr, "SSE cores mask invalid\n");
                exit (EXIT_FAILURE);
            }
            arg_index += 2;
        }
        else
        {
            fprintf (stderr, "Unknown argument %s\n", argv[arg_index]);
            exit (EXIT_FAILURE);
        }
    }

    /* Create an array of the test threads to create */
    const int num_cpus = numa_num_configured_cpus ();
    test_thread_context *const contexts = calloc (num_cpus, sizeof (test_thread_context));
    int num_threads = 0;
    int core;
    if (avx_cores_mask != NULL)
    {
        for (core = 0; core < num_cpus; core++)
        {
            if (numa_bitmask_isbitset (avx_cores_mask, core))
            {
                test_thread_context *const context = &contexts[num_threads];

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
                test_thread_context *const context = &contexts[num_threads];

                context->num_test_iterations = 0;
                context->core = core;
                context->avx_load_thread = false;
                num_threads++;
            }
        }
    }

    /* Create the test threads, with their CPU affinity set to the specified core and with their stack allocated from the
     *  local NUMA node.
     *  @todo doesn't set a guard for the allocated stack. */
    int thread_index;
    pthread_attr_t thread_attr;
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        test_thread_context *const context = &contexts[thread_index];
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
        test_thread_context *const context = &contexts[thread_index];
        void *thread_return;

        rc = pthread_join (context->thread_id, &thread_return);
        check_linux_status (rc, "pthread_join");
    }

    /* Display the statistics on the number of iterations performed by each thread */
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        const test_thread_context *const context = &contexts[thread_index];

        printf ("%s core %d num_test_iterations = %" PRIu64 " (final sum[0]=%.6g)\n",
                context->avx_load_thread ? "AVX" : "SSE", context->core, context->num_test_iterations, context->final_sum[0]);
    }

    return EXIT_SUCCESS;
}
