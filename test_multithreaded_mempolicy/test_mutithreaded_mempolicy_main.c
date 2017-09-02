/*
 * @file test_mutithreaded_mempolicy_main.c
 * @date 2 Sep 2017
 * @author Chester Gillon
 * @details Test calling set_mempolicy() from multiple threads to attempt to determine if the mempolicy applies
 *          to the process or calling thread.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <semaphore.h>
#include <pthread.h>
#include <numaif.h>

#define MAXNODE 64

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
        rc = set_mempolicy (mode, &nodemask, MAXNODE);
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
            fprintf (stderr, "set_mempolicy failed\n");
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

        rc = get_mempolicy (&mode, &nodemask, MAXNODE, NULL, 0);
        if (rc == 0)
        {
            printf ("  Thread %u get mempolicy ", context->thread_index);
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

            default:
                printf ("unknown mode %d", mode);
                break;
            }
            printf ("\n");
        }
        else
        {
            fprintf (stderr, "get_mempolicy failed\n");
            exit (EXIT_FAILURE);
        }

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
    unsigned long allowed_nodes_mask;
    unsigned int node_number;
    unsigned int thread_index;
    mempolicy_thread_context threads[NUM_MEMPOLICY_THREADS];
    unsigned int iteration;

    /* Get the allowed NUMA nodes mask, and calculate the number of nodes assuming start from zero */
    rc = get_mempolicy (NULL, &allowed_nodes_mask, MAXNODE, NULL, MPOL_F_MEMS_ALLOWED);
    if (rc != 0)
    {
        fprintf (stderr, "get_mem_policy failed\b");
        exit (EXIT_FAILURE);
    }

    num_nodes = 0;
    node_number = 0;
    while ((1u << node_number) & allowed_nodes_mask)
    {
        num_nodes++;
        node_number++;
    }
    printf ("Num NUMA nodes = %u\n", num_nodes);

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
