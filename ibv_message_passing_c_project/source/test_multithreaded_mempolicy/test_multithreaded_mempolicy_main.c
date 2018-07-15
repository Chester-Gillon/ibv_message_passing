/*
 * @file test_mutithreaded_mempolicy_main.c
 * @date 2 Sep 2017
 * @author Chester Gillon
 * @details Test calling set_mempolicy() from multiple threads to attempt to determine if the mempolicy applies
 *          to the process or calling thread.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <semaphore.h>
#include <errno.h>
#include <pthread.h>
#include <numa.h>
#include <numaif.h>

/** The different memory policies which are tested, in terms of if the polices are accepted and remembered
 *  rather than testing actual allocations. */
#define NUM_MODES_TESTED 4
static const int modes_tested[NUM_MODES_TESTED] =
{
    MPOL_DEFAULT,
    MPOL_BIND,
    MPOL_INTERLEAVE,
    MPOL_PREFERRED
};

/** The context for one thread which is used to test setting and retrieving the mempolicy */
#define NUM_MEMPOLICY_THREADS 4
typedef struct
{
    /** The thread index, for reporting in results */
    unsigned int thread_index;
    /** The ID of the thread */
    pthread_t pid;
    /** Semaphore which is given by the thread to indicate it is ready to perform the next action */
    sem_t ready_semaphore;
    /** Semaphore which is taken by the thread to perform the next test action */
    sem_t go_semaphore;
    /** The index into modes_tested[] which is the current mempolicy for the thread */
    unsigned int current_mode_index;
    /** Defines the current NUMA node for allocations in the mempolicy for the thread, which is
     *  valid when the mode is other than MPOL_DEFAULT. */
    unsigned int current_numa_node;
} mempolicy_thread_context;

/** The number of NUMA memory nodes */
static unsigned int num_nodes;

/** unsigned long array length for get_mempolicy() call to store the nodemask length
 *  allocated by the Kernel */
static int nodemask_array_length;

/** maxnode value for get_mempolicy() for nodemask_array_length */
static int maxnode;

/**
 * @brief Obtain and display the current mempolicy for the calling thread
 * @param[in] is_main_true True if being called from the program main thread
 * @param[in] thread_index If is_main_thread is false, the index of which test thread is called from
 */
static void display_current_mempolicy (const bool is_main_thread, const unsigned int thread_index)
{
    int rc;
    int mode;
    unsigned long *nodemask = calloc (nodemask_array_length, sizeof (unsigned long));
    bool nodemask_valid;

    errno = 0;
    rc = get_mempolicy (&mode, nodemask, maxnode, NULL, 0);
    nodemask_valid = rc == 0;
    if ((rc != 0) && (errno == EINVAL))
    {
        rc = get_mempolicy (&mode, NULL, 0, NULL, 0);
    }
    if (rc == 0)
    {
        if (is_main_thread)
        {
            printf ("  Main thread get mempolicy ");
        }
        else
        {
            printf ("  Thread %u get mempolicy ", thread_index);
        }
        switch (mode)
        {
        case MPOL_DEFAULT:
            printf ("mode=MPOL_DEFAULT");
            break;

        case MPOL_BIND:
            if (nodemask_valid)
            {
                printf ("mode=MPOL_BIND nodemask=%lu", *nodemask);
            }
            else
            {
                printf ("mode=MPOL_BIND nodemask=???");
            }
            break;

        case MPOL_INTERLEAVE:
            if (nodemask_valid)
            {
                printf ("mode=MPOL_INTERLEAVE nodemask=%lu", *nodemask);
            }
            else
            {
                printf ("mode=MPOL_INTERLEAVE nodemask=???");
            }
            break;

        case MPOL_PREFERRED:
            if (nodemask_valid)
            {
                printf ("mode=MPOL_PREFERRED nodemask=%lu", *nodemask);
            }
            else
            {
                printf ("mode=MPOL_PREFERRED nodemask=???");
            }
            break;

        default:
            printf ("unknown mode %d", mode);
            break;
        }
        printf ("\n");
    }
    else
    {
        perror ("get_mempolicy failed\n");
        exit (EXIT_FAILURE);
    }

    free (nodemask);
}

/**
 * @brief The test thread used to set / get its mempolicy under when instructed by the main thread
 * @details If set_mempolicy sets a per-thead mempolicy it is expected that get_mempolicy will return the
 *          per-thread mempolicy, where each thread have their own policy
 * @params[in,out] arg The context for the thread
 * @return Not used
 */
static void *mempolicy_thread (void *const arg)
{
    mempolicy_thread_context *const context = (mempolicy_thread_context *) arg;
    int rc;
    int mode;
    unsigned long nodemask;

    /* Tell the main thread we have initialised */
    rc = sem_post (&context->ready_semaphore);
    if (rc != 0)
    {
        fprintf (stderr, "sem_post failed\n");
        exit (EXIT_FAILURE);
    }

    for (;;)
    {
        /* Set the mempolicy when instructed by the main thread */
        rc = sem_wait (&context->go_semaphore);
        if (rc != 0)
        {
            fprintf (stderr, "sem_wait failed\n");
            exit (EXIT_FAILURE);
        }

        mode = modes_tested[context->current_mode_index];
        if (mode != MPOL_DEFAULT)
        {
            nodemask = 1u << context->current_numa_node;
        }
        else
        {
            nodemask = 0;
        }
        rc = set_mempolicy (mode, (nodemask != 0) ? &nodemask : NULL, numa_max_possible_node());
        if (rc == 0)
        {
            printf ("  Thread %u set mempolicy ", context->thread_index);
            switch (mode)
            {
            case MPOL_DEFAULT:
                printf ("mode=MPOL_DEFAULT");
                break;

            case MPOL_BIND:
                printf ("mode=MPOL_BIND nodemask=%lu", nodemask);
                break;

            case MPOL_INTERLEAVE:
                printf ("mode=MPOL_INTERLEAVE nodemask=%lu", nodemask);
                break;

            case MPOL_PREFERRED:
                printf ("mode=MPOL_PREFERRED nodemask=%lu", nodemask);
                break;
            }
            printf ("\n");
        }
        else
        {
            perror ("set_mempolicy failed\n");
            exit (EXIT_FAILURE);
        }

        rc = sem_post (&context->ready_semaphore);
        if (rc != 0)
        {
            fprintf (stderr, "sem_post failed\n");
            exit (EXIT_FAILURE);
        }

        /* Readback and report the current mempolicy when instructed by the main thread */
        rc = sem_wait (&context->go_semaphore);
        if (rc != 0)
        {
            fprintf (stderr, "sem_wait failed\n");
            exit (EXIT_FAILURE);
        }

        display_current_mempolicy (false, context->thread_index);

        rc = sem_post (&context->ready_semaphore);
        if (rc != 0)
        {
            fprintf (stderr, "sem_post failed\n");
            exit (EXIT_FAILURE);
        }

        /* Advance the mode to be set for the next test */
        context->current_mode_index = (context->current_mode_index + 1) % NUM_MODES_TESTED;
        if (context->current_mode_index != MPOL_DEFAULT)
        {
            context->current_numa_node = (context->current_numa_node + 1) % num_nodes;
        }
    }

    return NULL;
}

int main (int argc, char *argv[])
{
    int rc;
    unsigned int thread_index;
    mempolicy_thread_context threads[NUM_MEMPOLICY_THREADS];
    unsigned int iteration;
    const int unsigned_long_bits = sizeof (unsigned long) * 8;
    char text_buffer[2048];

    /* Get the number of NUMA nodes */
    rc = numa_available ();
    if (rc == -1)
    {
        fprintf (stderr, "NUMA is not available\n");
        exit (EXIT_FAILURE);
    }
    num_nodes = numa_num_configured_nodes ();
    snprintf (text_buffer, sizeof (text_buffer), "Num NUMA nodes = %u\nnuma_max_possible_node=%d\n",
    		  num_nodes, numa_max_possible_node ());
    printf ("%s", text_buffer);
    maxnode = numa_max_possible_node() + 1;
    nodemask_array_length = (maxnode + unsigned_long_bits - 1) / unsigned_long_bits;
    if (num_nodes > unsigned_long_bits)
    {
        fprintf (stderr, "The number of NUMA nodes exceeds the number of bits in an unsigned long\n");
        fprintf (stderr, "This program has been written assuming the nodemask can be set in a single unsigned long\n");
        exit (EXIT_FAILURE);
    }

    /* Create the test threads, and wait form them to be ready */
    memset (threads, 0, sizeof (threads));
    for (thread_index = 0; thread_index < NUM_MEMPOLICY_THREADS; thread_index++)
    {
        mempolicy_thread_context *const thread = &threads[thread_index];

        thread->thread_index = thread_index;
        rc = sem_init (&thread->ready_semaphore, 0, 0);
        if (rc != 0)
        {
            fprintf (stderr, "sem_init failed\n");
            exit (EXIT_FAILURE);
        }
        rc = sem_init (&thread->go_semaphore, 0, 0);
        if (rc != 0)
        {
            fprintf (stderr, "sem_init failed\n");
            exit (EXIT_FAILURE);
        }
        thread->current_mode_index = thread_index % NUM_MODES_TESTED;
        thread->current_numa_node = thread_index % num_nodes;
        rc = pthread_create (&thread->pid, NULL, mempolicy_thread, thread);
        if (rc != 0)
        {
            fprintf (stderr, "pthread_create failed\n");
            exit (EXIT_FAILURE);
        }

        rc = sem_wait (&thread->ready_semaphore);
        if (rc != 0)
        {
            fprintf (stderr, "sem_wait failed\n");
            exit (EXIT_FAILURE);
        }
    }

    /* Run the test of setting each thread to a different mempolicy, and then getting the threads to report their current mempolicy */
    for (iteration = 0; iteration < 10; iteration++)
    {
        printf ("Iteration %u:\n", iteration);

        /* Tell the threads to set a mempolicy */
        for (thread_index = 0; thread_index < NUM_MEMPOLICY_THREADS; thread_index++)
        {
            mempolicy_thread_context *const thread = &threads[thread_index];

            rc = sem_post (&thread->go_semaphore);
            if (rc != 0)
            {
                fprintf (stderr, "sem_post failed\n");
                exit (EXIT_FAILURE);
            }

            rc = sem_wait (&thread->ready_semaphore);
            if (rc != 0)
            {
                fprintf (stderr, "sem_wait failed\n");
                exit (EXIT_FAILURE);
            }
        }

        /* Tell the threads to readback and display their mempolicy */
        display_current_mempolicy (true, 0);
        for (thread_index = 0; thread_index < NUM_MEMPOLICY_THREADS; thread_index++)
        {
            mempolicy_thread_context *const thread = &threads[thread_index];

            rc = sem_post (&thread->go_semaphore);
            if (rc != 0)
            {
                fprintf (stderr, "sem_post failed\n");
                exit (EXIT_FAILURE);
            }

            rc = sem_wait (&thread->ready_semaphore);
            if (rc != 0)
            {
                fprintf (stderr, "sem_wait failed\n");
                exit (EXIT_FAILURE);
            }
        }
    }

    return EXIT_SUCCESS;
}
