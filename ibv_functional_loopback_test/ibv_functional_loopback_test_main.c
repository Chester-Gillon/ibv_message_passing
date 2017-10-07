/*
 * @file ibv_functional_loopback_test_main.c
 * @date 3 Sep 2017
 * @author Chester Gillon
 * @details Perform functional Infiniband message passing tests with a single threaded program,
 *          assuming a dual port Infiniband adapter with port 1 looped to port 2.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <semaphore.h>
#include <time.h>
#include <pthread.h>
#include <sys/resource.h>
#include <infiniband/verbs.h>

#include "ibv_utils.h"
#include "ibv_functional_loopback_test_interface.h"

/** The list of all Infiniband devices */
static int num_ibv_devices;
static struct ibv_device **ibv_device_list;

/** The Infiniband device used for the loopback tests, which is the first device found */
struct ibv_context *ibv_loopback_device;
struct ibv_device_attr ibv_loopback_device_attributes;

/** The ports used on the Infiniband device used for the loopback tests */
struct ibv_port_attr ibv_loopback_port_attributes[NUM_TEST_PORTS+1];

/** The protection domain for the Infiniband device used for the loopback tests */
struct ibv_pd *ibv_loopback_device_pd;

/** Used to collect statistics for the Infiniband ports used for the loopback tests */
static infiniband_statistics_handle ibv_statistics_handle;

/** For the tests using a sender and receiver thread, defines the message IDs passed to the receiver */
typedef enum
{
    /** The receiver thread exits after freeing the message */
    RX_THREAD_EXIT,
    /** The receiver thread ends the current test after freeing the message */
    RX_THREAD_END_TEST,
    /** The receiver thread frees the message without any delay, for testing the message rate */
    RX_THREAD_FREE_MSG_IMMEDIATELY,
    /** The receiver thread frees the message after a specified delay.
     *  This is for testing that flow-control blocks the sender if the receiver can't keep up.
     *  The delay in microseconds is in the source_instance of the message header. */
    RX_THREAD_FREE_MSG_AFTER_DELAY
} test_threads_message_id;

/** Defines one test performed using a sender and receiver thread.
 *  The test definition is used by the sender thread, and the receiver thread responds to received messages. */
typedef struct
{
    /** The number of seconds the test runs for */
    uint32_t test_duration_seconds;
    /** The data length for each message sent */
    uint32_t data_length_bytes;
    /** The number of nanoseconds the receive thread will delay before freeing a message */
    uint64_t receive_thread_free_delay_ns;
    /** Even one in thread_free_delay_interval messages the receive thread will delay for receive_thread_free_delay_us */
    uint32_t receive_thread_free_delay_interval;
} thread_test_definition;

/** The test results generated by a sender thread */
typedef struct
{
    /** The CLOCK_MONOTONIC time when the first test message was queued for transmission */
    struct timespec time_first_message_queued;
    /** The number of time the receive thread was requested to delay before freeing a message */
    uint64_t num_receiver_delays_requested;
    /** The maximum number of nanoseconds the sender blocked waiting for get_send_buffer() to get a buffer.
     *  This is only measured when test_definition.receive_thread_free_delay_ns is non-zero,
     *  and may be influenced by the sender or receiver threads being preempted rather than due to a
     *  deliberate delay in the receiver. */
    uint64_t max_get_send_buffer_duration_ns;
} sender_thread_results;

/** Defines the context for a test thread which sends test messages */
typedef struct
{
    /** The identity of the thread */
    pthread_t id;
    /** Given by the test thread to indicate it is ready for the next test */
    sem_t ready_sem;
    /** Given by the main thread to tell the test thread to start the next test */
    sem_t go_sem;
    /** Contains pointers to the message send and receive functions to be tested */
    const message_communication_functions *comms_functions;
    /** Set to cause the sender and receiver threads to exit */
    bool exit_threads;
    /** Defines the current test to be performed, updated by the main thread before posting the go_sem */
    thread_test_definition test_definition;
    /** The send context to send the messages on */
    api_send_context send_context;
    /** The results for the current test */
    sender_thread_results test_results;
} sender_thread_context;

/** The test results generated by a receiver thread */
typedef struct
{
    /** The CLOCK_MONOTONIC time when the last test message was freed, which when compared with the sender time_first_message_queued
     *  gives the elapsed time for transferring the test messages */
    struct timespec time_last_message_freed;
    /** The total number of messages which were received during the test */
    uint64_t total_messages_received;
    /** The total number of message data bytes which were received during the test (excludes the header bytes) */
    uint64_t total_data_bytes_received;
} receiver_thread_results;

/** Defines the context for a test thread which receives test messages */
typedef struct
{
    /** The identity of the thread */
    pthread_t id;
    /** Given by the test thread to indicate it is ready for the next test */
    sem_t ready_sem;
    /** Given by the main thread to tell the test thread to start the next test */
    sem_t go_sem;
    /** Contains pointers to the message send and receive functions to be tested */
    const message_communication_functions *comms_functions;
    /** The receive context to receive the messages from */
    api_receive_context receive_context;
    /** The results for the current test */
    receiver_thread_results test_results;
} receiver_thread_context;

/** Contains the results for one message transfer test using a sender and receiver thread */
typedef struct
{
    /** Defines the test which has been performed */
    thread_test_definition test_definition;
    /** The results from the sender thread */
    sender_thread_results sender_results;
    /** The results from the receiver thread */
    receiver_thread_results receiver_results;
} message_thread_results;

/** The different tests which are run for the message tests using thread */
typedef enum
{
    /** Send test messages with the receiver inserting short delays of microseconds between freeing messages */
    THREAD_TEST_SHORT_FREE_DELAY,
    /** Send test messages with the receiver inserting long delays of milliseconds between freeing messages */
    THREAD_TEST_LONG_FREE_DELAY,
    /** Send header-only messages with zero data bytes */
    THREAD_TEST_ZERO_DATA_BYTES,
    /** Send messages the number of message bytes increasing in powers of two from one to the maximum */
    THREAD_TEST_LOG2N_MIN_DATA_BYTES,
    THREAD_TEST_LOG2N_MAX_DATA_BYTES = THREAD_TEST_LOG2N_MIN_DATA_BYTES + MAX_MESSAGE_DATA_LEN_LOG2N,
    THREAD_NUM_TESTS
} thread_test_type;

/**
 * @brief Open the Infiniband device to be used for the loopback tests, including obtaining the port attributes
 */
static void open_infiniband_loopback_ports (void)
{
    int rc;
    int port_num;

    /* Find all Infiniband devices */
    ibv_device_list = ibv_get_device_list (&num_ibv_devices);
    if (ibv_device_list == NULL)
    {
        perror ("ibv_get_device_list failed");
        exit (EXIT_FAILURE);
    }
    if (num_ibv_devices == 0)
    {
        fprintf (stderr, "No Infiniband devices found\n");
        exit (EXIT_FAILURE);
    }

    /* Open the first device for use in the loopback tests */
    ibv_loopback_device = ibv_open_device (ibv_device_list[0]);
    if (ibv_loopback_device == NULL)
    {
        fprintf (stderr, "ibv_open_device failed\n");
        exit (EXIT_FAILURE);
    }

    /* Display the attributes of the loopback device */
    rc = ibv_query_device (ibv_loopback_device, &ibv_loopback_device_attributes);
    if (rc != 0)
    {
        perror ("ibv_query_device failed");
        exit (EXIT_FAILURE);
    }
    display_ibv_device_attributes (ibv_loopback_device, &ibv_loopback_device_attributes);

    /* Display the attributes of the ports used for the loopback tests */
    if (ibv_loopback_device_attributes.phys_port_cnt < NUM_TEST_PORTS)
    {
        fprintf (stderr, "Insufficient Infiniband ports on device for loopback test\n");
        exit (EXIT_FAILURE);
    }
    for (port_num = 1; port_num <= NUM_TEST_PORTS; port_num++)
    {
        rc = ibv_query_port (ibv_loopback_device, port_num, &ibv_loopback_port_attributes[port_num]);
        if (rc != 0)
        {
            perror ("ibv_query_port failed");
            exit (EXIT_FAILURE);
        }
    }
    printf ("Attributes of source port %u:\n", SOURCE_PORT_NUM);
    display_ibv_port_attributes (&ibv_loopback_port_attributes[SOURCE_PORT_NUM]);
    printf ("Attributes of destination port %u:\n", DESTINATION_PORT_NUM);
    display_ibv_port_attributes (&ibv_loopback_port_attributes[DESTINATION_PORT_NUM]);

    /* Create the single protection domain used for all tests */
    ibv_loopback_device_pd = ibv_alloc_pd (ibv_loopback_device);
    if (ibv_loopback_device_pd == NULL)
    {
        fprintf (stderr, "ibv_alloc_pd failed\n");
        exit (EXIT_SUCCESS);
    }

    open_infiniband_statistics_handle (&ibv_statistics_handle, ibv_loopback_device, &ibv_loopback_device_attributes,
                                       ibv_loopback_port_attributes);
}

/**
 * @brief Close the Infiniband device used for the loopback tests
 */
static void close_ininiband_loopback_ports (void)
{
    int rc;

    close_infiniband_statistics_handle (&ibv_statistics_handle);

    rc = ibv_dealloc_pd (ibv_loopback_device_pd);
    if (rc != 0)
    {
        perror ("ibv_dealloc_pd failed");
        exit (EXIT_FAILURE);
    }

    rc = ibv_close_device (ibv_loopback_device);
    if (rc != 0)
    {
        fprintf (stderr, "ibv_close_device failed\n");
        exit (EXIT_SUCCESS);
    }
    ibv_free_device_list (ibv_device_list);
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
 * @brief Perform one test for a message sender thread, running for the specified test duration
 * @param[in,out] thread_context The sender thread context
 */
static void perform_message_sender_test (sender_thread_context *const thread_context)
{
    struct timespec start_time;
    struct timespec end_time;
    struct timespec now;
    struct timespec get_buffer_start = {0};
    bool test_duration_complete;
    uint64_t total_messages = 0;

    thread_context->test_results.num_receiver_delays_requested = 0;
    thread_context->test_results.max_get_send_buffer_duration_ns = 0;
    clock_gettime (CLOCK_MONOTONIC, &start_time);
    end_time = start_time;
    end_time.tv_sec += thread_context->test_definition.test_duration_seconds;

    do
    {
        api_message_buffer *tx_buffer;

        if (thread_context->test_definition.receive_thread_free_delay_ns > 0)
        {
            /* Tell the receiver to delay freeing a message at a regular message rate */
            clock_gettime (CLOCK_MONOTONIC, &get_buffer_start);
            tx_buffer = thread_context->comms_functions->get_send_buffer (thread_context->send_context);
            clock_gettime (CLOCK_MONOTONIC, &now);
            tx_buffer->message->header.source_instance = thread_context->test_definition.receive_thread_free_delay_ns;
            if ((total_messages % thread_context->test_definition.receive_thread_free_delay_interval) == 0)
            {
                tx_buffer->message->header.message_id = RX_THREAD_FREE_MSG_AFTER_DELAY;
                thread_context->test_results.num_receiver_delays_requested++;
            }
            else
            {
                tx_buffer->message->header.message_id = RX_THREAD_FREE_MSG_IMMEDIATELY;
            }
        }
        else
        {
            /* Tell the receiver to free all messages immediately */
            tx_buffer = thread_context->comms_functions->get_send_buffer (thread_context->send_context);
            tx_buffer->message->header.message_id = RX_THREAD_FREE_MSG_IMMEDIATELY;
            clock_gettime (CLOCK_MONOTONIC, &now);
        }

        /* Send the message */
        test_duration_complete = (now.tv_sec >= end_time.tv_sec) && (now.tv_nsec >= end_time.tv_nsec);
        tx_buffer->message->header.message_length = thread_context->test_definition.data_length_bytes;
        if (test_duration_complete)
        {
            tx_buffer->message->header.message_id = RX_THREAD_END_TEST;
        }
        thread_context->comms_functions->send_message (thread_context->send_context, tx_buffer);
        if (total_messages == 0)
        {
            clock_gettime (CLOCK_MONOTONIC, &thread_context->test_results.time_first_message_queued);
        }
        total_messages++;

        /* Update the maximum time taken to get a message buffer (when the receiver may delay before freeing a message) */
        if (thread_context->test_definition.receive_thread_free_delay_ns > 0)
        {
            const uint64_t get_send_buffer_duration_ns = get_elapsed_ns (&get_buffer_start, &now);

            if (get_send_buffer_duration_ns > thread_context->test_results.max_get_send_buffer_duration_ns)
            {
                thread_context->test_results.max_get_send_buffer_duration_ns = get_send_buffer_duration_ns;
            }
        }
    } while (!test_duration_complete);
}

/**
 * @brief The entry point for a test thread which sends messages to a receiver thread
 * @param[in,out] arg The context for the sender test thread
 * @return Not used
 */
static void *message_sender_thread (void *const arg)
{
    sender_thread_context *const thread_context = (sender_thread_context *) arg;
    int rc;
    bool exit_thread = false;

    /* Indicate the thread is running */
    rc = sem_post (&thread_context->ready_sem);
    CHECK_ASSERT (rc == 0);

    /* Run tests until told to complete */
    while (!exit_thread)
    {
        /* Wait to be told to start the next test */
        rc = sem_wait (&thread_context->go_sem);
        CHECK_ASSERT (rc == 0);
        exit_thread = thread_context->exit_threads;

        if (exit_thread)
        {
            /* Forward the exit request to the receive thread */
            api_message_buffer *const tx_buffer = thread_context->comms_functions->get_send_buffer (thread_context->send_context);

            tx_buffer->message->header.message_id = RX_THREAD_EXIT;
            tx_buffer->message->header.message_length = 0;
            thread_context->comms_functions->send_message (thread_context->send_context, tx_buffer);
        }
        else
        {
            perform_message_sender_test (thread_context);
        }

        /* Report the test is complete */
        rc = sem_post (&thread_context->ready_sem);
        CHECK_ASSERT (rc == 0);
    }

    return NULL;
}

/**
 * @brief Perform one test for a message receiver thread, running until told the stop the test by the sender
 * @param[in,out] thread_context The receiver thread context
 * @return Returns true if the receiver thread is to exit, or false if to run another test
 */
static bool perform_message_receiver_test (receiver_thread_context *const thread_context)
{
    bool exit_thread = false;
    bool test_complete = false;

    thread_context->test_results.total_messages_received = 0;
    thread_context->test_results.total_data_bytes_received = 0;
    while (!test_complete)
    {
        api_message_buffer *const rx_buffer = thread_context->comms_functions->await_message (thread_context->receive_context);

        switch (rx_buffer->message->header.message_id)
        {
        case RX_THREAD_EXIT:
            test_complete = true;
            exit_thread = true;
            break;

        case RX_THREAD_END_TEST:
            test_complete = true;
            break;

        case RX_THREAD_FREE_MSG_IMMEDIATELY:
            /* No action required */
            break;

        case RX_THREAD_FREE_MSG_AFTER_DELAY:
            /* Delay for the specified amount of nanoseconds, specified by the source_instance field in the header */
            {
                struct timespec start_time;
                struct timespec now;

                clock_gettime (CLOCK_MONOTONIC, &start_time);
                do
                {
                    clock_gettime (CLOCK_MONOTONIC, &now);
                } while (get_elapsed_ns (&start_time, &now) < rx_buffer->message->header.source_instance);
            }
            break;

        default:
            check_assert (false, "Unknown message_id in perform_message_receiver_test");
            break;
        }

        thread_context->test_results.total_messages_received++;
        thread_context->test_results.total_data_bytes_received += rx_buffer->message->header.message_length;
        thread_context->comms_functions->free_message (thread_context->receive_context, rx_buffer);
    }
    clock_gettime (CLOCK_MONOTONIC, &thread_context->test_results.time_last_message_freed);

    return exit_thread;
}

/**
 * @brief The entry point for a test thread which receives test messages
 * @param[in,out] arg The context for the receiver test thread
 * @return Not used
 */
static void *message_receiver_thread (void *const arg)
{
    receiver_thread_context *const thread_context = (receiver_thread_context *) arg;
    int rc;
    bool exit_thread = false;

    /* Indicate the thread is running */
    rc = sem_post (&thread_context->ready_sem);
    CHECK_ASSERT (rc == 0);

    /* Run tests until told to complete */
    while (!exit_thread)
    {
        /* Wait to be told to start the next test */
        rc = sem_wait (&thread_context->go_sem);
        CHECK_ASSERT (rc == 0);

        exit_thread = perform_message_receiver_test (thread_context);

        /* Report the test is complete */
        rc = sem_post (&thread_context->ready_sem);
        CHECK_ASSERT (rc == 0);
    }

    return NULL;
}

/**
 * @brief Create a message sender thread, which is initially idle
 * @param[in] comms_functions The communication functions to be used by the thread
 * @return Returns the allocated thread context
 */
static sender_thread_context *create_message_sender_thread (const message_communication_functions *const comms_functions)
{
    sender_thread_context *const send_thread = cache_line_aligned_calloc (1, sizeof (sender_thread_context));
    int rc;

    rc = sem_init (&send_thread->go_sem, 0, 0);
    CHECK_ASSERT (rc == 0);
    rc = sem_init (&send_thread->ready_sem, 0, 0);
    CHECK_ASSERT (rc == 0);
    send_thread->comms_functions = comms_functions;

    rc = pthread_create (&send_thread->id, NULL, message_sender_thread, send_thread);
    CHECK_ASSERT (rc == 0);

    rc = sem_wait (&send_thread->ready_sem);
    CHECK_ASSERT (rc == 0);

    return send_thread;
}

/**
 * @brief Create a message receiver thread, which is initially idle
 * @param[in] comms_functions The communication functions to be used by the thread
 * @return Returns the allocated thread context
 */
static receiver_thread_context *create_message_receiver_thread (const message_communication_functions *const comms_functions)
{
    receiver_thread_context *const receive_thread = cache_line_aligned_calloc (1, sizeof (receiver_thread_context));
    int rc;

    rc = sem_init (&receive_thread->go_sem, 0, 0);
    CHECK_ASSERT (rc == 0);
    rc = sem_init (&receive_thread->ready_sem, 0, 0);
    CHECK_ASSERT (rc == 0);
    receive_thread->comms_functions = comms_functions;

    rc = pthread_create (&receive_thread->id, NULL, message_receiver_thread, receive_thread);
    CHECK_ASSERT (rc == 0);

    rc = sem_wait (&receive_thread->ready_sem);
    CHECK_ASSERT (rc == 0);

    return receive_thread;
}

/**
 * @brief Cause the message send and receive test threads to exit, and free their resources
 * @param[in,out] send_thread The message sender thread to make exit
 * @param[in,out] receive_thread The message receiver thread to make exit
 */
static void terminate_test_threads (sender_thread_context *const send_thread, receiver_thread_context *const receive_thread)
{
    int rc;
    void *thread_return;

    /* Tell the thread to exit */
    send_thread->exit_threads = true;
    rc = sem_post (&receive_thread->go_sem);
    CHECK_ASSERT (rc == 0);
    rc = sem_post (&send_thread->go_sem);
    CHECK_ASSERT (rc == 0);

    /* Await threads to indicate about to exit */
    rc = sem_wait (&receive_thread->ready_sem);
    CHECK_ASSERT (rc == 0);
    rc = sem_wait (&send_thread->ready_sem);
    CHECK_ASSERT (rc == 0);

    /* Wait for threads to exit */
    rc = pthread_join (receive_thread->id, &thread_return);
    CHECK_ASSERT (rc == 0);
    rc = pthread_join (send_thread->id, &thread_return);
    CHECK_ASSERT (rc == 0);

    /* Release thread resources */
    rc = sem_destroy (&receive_thread->go_sem);
    CHECK_ASSERT (rc == 0);
    rc = sem_destroy (&receive_thread->ready_sem);
    CHECK_ASSERT (rc == 0);
    free (receive_thread);

    rc = sem_destroy (&send_thread->go_sem);
    CHECK_ASSERT (rc == 0);
    rc = sem_destroy (&send_thread->ready_sem);
    CHECK_ASSERT (rc == 0);
    free (send_thread);
}

/**
 * @brief Cause the message sender and receiver threads to perform one test
 * @param[in,out] send_thread The message sender thread to use for the test
 * @param[in,out] receive_thread The message receiver thread to use for the test
 * @param[in] test_definition Defines the test to perform
 * @param[out] test_result The collated results for the test
 */
static void perform_message_thread_test (sender_thread_context *const send_thread, receiver_thread_context *const receive_thread,
                                         const thread_test_definition *const test_definition,
                                         message_thread_results *const test_results)
{
    int rc;

    /* Start the test */
    send_thread->exit_threads = false;
    send_thread->test_definition = *test_definition;
    rc = sem_post (&receive_thread->go_sem);
    CHECK_ASSERT (rc == 0);
    rc = sem_post (&send_thread->go_sem);
    CHECK_ASSERT (rc == 0);

    /* Await test completion */
    rc = sem_wait (&receive_thread->ready_sem);
    CHECK_ASSERT (rc == 0);
    rc = sem_wait (&send_thread->ready_sem);
    CHECK_ASSERT (rc == 0);

    /* Collate the results */
    test_results->test_definition = *test_definition;
    test_results->sender_results = send_thread->test_results;
    test_results->receiver_results = receive_thread->test_results;
}

/**
 * @brief Initialise the definition of the tests to be performed by the message sender and receiver threads
 * @param[out] thread_test_definitions The test definitions which have been initialised
 */
static void initialise_message_thread_tests (thread_test_definition thread_test_definitions[THREAD_NUM_TESTS])
{
    int test_index;

    memset (thread_test_definitions, 0, sizeof (thread_test_definition) * THREAD_NUM_TESTS);
    for (test_index = 0; test_index < THREAD_NUM_TESTS; test_index++)
    {
        thread_test_definition *const definition = &thread_test_definitions[test_index];

        switch (test_index)
        {
        case THREAD_TEST_SHORT_FREE_DELAY:
            definition->data_length_bytes = 0;
            definition->test_duration_seconds = 1;
            definition->receive_thread_free_delay_ns = 10000; /* 10 microseconds */
            definition->receive_thread_free_delay_interval = 1000;
            break;

        case THREAD_TEST_LONG_FREE_DELAY:
            definition->data_length_bytes = 16;
            definition->test_duration_seconds = 1;
            definition->receive_thread_free_delay_ns = 10000000; /* 10 milliseconds */
            definition->receive_thread_free_delay_interval = 1000;
            break;

        case THREAD_TEST_ZERO_DATA_BYTES:
            definition->data_length_bytes = 0;
            definition->test_duration_seconds = 1;
            definition->receive_thread_free_delay_ns = 0;
            definition->receive_thread_free_delay_interval = 0;
            break;

        default:
            definition->data_length_bytes = 1u << (test_index - THREAD_TEST_LOG2N_MIN_DATA_BYTES);
            definition->test_duration_seconds = 1;
            definition->receive_thread_free_delay_ns = 0;
            definition->receive_thread_free_delay_interval = 0;
            break;
        }
    }
}

/**
 * @brief Write the results for the message sender and receiver threads to standard out in tabular form
 * @param[in] thread_test_results Contains the test results to report
 */
static void report_message_thread_test_results (const message_thread_results thread_test_results[THREAD_NUM_TESTS])
{
    const char *column_headers[] =
    {
        "Test duration (secs)",
        "Data length (bytes)",
        "Total messages received",
        "Total data bytes received",
        "Messages per sec",
        "Data bytes per sec",
        "Rx thread free delay (us)",
        "Tx thread max get send buffer (us)"
    };
    const int num_cols = sizeof(column_headers) / sizeof (column_headers[0]);
    int col_index;
    int test_index;

    /* Write column headers */
    printf ("\n");
    for (col_index = 0; col_index < num_cols; col_index++)
    {
        printf ("%s  ", column_headers[col_index]);
    }
    printf ("\n");

    /* Write the test results */
    for (test_index = 0; test_index < THREAD_NUM_TESTS; test_index++)
    {
        const message_thread_results *const results = &thread_test_results[test_index];
        const double test_duration_secs =
                get_elapsed_ns (&results->sender_results.time_first_message_queued,
                                &results->receiver_results.time_last_message_freed) / 1E9;
        const double messages_per_sec = (double) results->receiver_results.total_messages_received / test_duration_secs;
        const double bytes_per_sec = (double) results->receiver_results.total_data_bytes_received / test_duration_secs;

        col_index = 0;
        printf ("%*.6f  ", (int) strlen (column_headers[col_index++]), test_duration_secs);
        printf ("%*u  ", (int) strlen (column_headers[col_index++]), results->test_definition.data_length_bytes);
        printf ("%*lu  ", (int) strlen (column_headers[col_index++]), results->receiver_results.total_messages_received);
        printf ("%*lu  ", (int) strlen (column_headers[col_index++]), results->receiver_results.total_data_bytes_received);
        printf ("%*.0f  ", (int) strlen (column_headers[col_index++]), messages_per_sec);
        printf ("%*.0f  ", (int) strlen (column_headers[col_index++]), bytes_per_sec);
        if (results->test_definition.receive_thread_free_delay_ns > 0)
        {
            printf ("%*lu  ", (int) strlen (column_headers[col_index++]),
                    results->test_definition.receive_thread_free_delay_ns / 1000);
            printf ("%*lu  ", (int) strlen (column_headers[col_index++]),
                    results->sender_results.max_get_send_buffer_duration_ns / 1000);
        }
        printf ("\n");
    }
}

/**
 * @details Perform a test of transferring messages of increasing size, from the minimum up to the maximum.
 *          The sending and reception is performed with a single thread, which queues a number of messages
 *          for transmission and then waits for reception of the messages.
 *
 *          As a result of using a single thread, by the time the sender needs to reuse a message buffer
 *          the message buffer will have already been freed so doesn't completely demonstrate that flow control
 *          blocks the sender.
 *
 *          The messages contain an incrementing test pattern which is checked on receipt.
 *
 *          When num_overlapped_messages is greater than one the order of calls is adjusted to test:
 *          a) send_message() being called in a different order to the buffers were obtained with get_send_buffer().
 *          b) free_message() being called in a different order to the buffers were received with await_message().
 * @param[in] comms_functions Contains pointers to the message send and receive functions to be tested
 * @param[in,out] send_context The send context to send the messages on
 * @param[in,out] receive_context The receive context to receive the messages on
 * @param[in] num_overlapped_messages The number of messages which are sent / received in each test iteration.
 *                                    Valid range is 1..NUM_MESSAGE_BUFFERS
 */
static void increasing_message_size_test (const message_communication_functions *const comms_functions,
                                          api_send_context send_context, api_receive_context receive_context,
                                          const uint32_t num_overlapped_messages)
{
    const uint32_t data_length_increment_words = 32767;
    uint32_t next_data_length_bytes = 0;
    uint32_t next_test_pattern_value = 0;
    uint32_t data_lengths_bytes[NUM_MESSAGE_BUFFERS] = {0};
    uint32_t data_lengths_words[NUM_MESSAGE_BUFFERS] = {0};
    uint32_t test_pattern_start_values[NUM_MESSAGE_BUFFERS] = {0};
    api_message_buffer *send_buffers[NUM_MESSAGE_BUFFERS] = {0};
    api_message_buffer *receive_buffers[NUM_MESSAGE_BUFFERS] = {0};
    unsigned int num_messages_in_iteration;
    unsigned int message_index;
    unsigned int test_pattern_index;
    unsigned int send_order_offset = 0;
    unsigned int free_order_offset = 1;

    while (next_data_length_bytes < MAX_MESSAGE_DATA_LEN_BYTES)
    {
        /* Determine the number of sizes of messages to be used for the next iteration */
        num_messages_in_iteration = 0;
        while ((next_data_length_bytes < MAX_MESSAGE_DATA_LEN_BYTES) && (num_messages_in_iteration < num_overlapped_messages))
        {
            data_lengths_bytes[num_messages_in_iteration] = next_data_length_bytes;
            data_lengths_words[num_messages_in_iteration] = next_data_length_bytes / sizeof (uint32_t);
            test_pattern_start_values[num_messages_in_iteration] = next_test_pattern_value;
            next_test_pattern_value += data_length_increment_words;
            next_data_length_bytes += (data_length_increment_words * sizeof (uint32_t));
            if (next_data_length_bytes > MAX_MESSAGE_DATA_LEN_BYTES)
            {
                next_data_length_bytes = MAX_MESSAGE_DATA_LEN_BYTES;
            }
            num_messages_in_iteration++;
        }

        /* Get the message send buffers */
        for (message_index = 0; message_index < num_messages_in_iteration; message_index++)
        {
            send_buffers[message_index] = comms_functions->get_send_buffer (send_context);
        }

        /* Populate the test messages to be sent */
        for (message_index = 0; message_index < num_messages_in_iteration; message_index++)
        {
            test_message *const message = send_buffers[message_index]->message;

            message->header.message_length = data_lengths_bytes[message_index];
            message->header.message_id = test_pattern_start_values[message_index];
            message->header.source_instance = test_pattern_start_values[message_index];
            for (test_pattern_index = 0; test_pattern_index < data_lengths_words[message_index]; test_pattern_index++)
            {
                message->data[test_pattern_index] = test_pattern_start_values[message_index] + test_pattern_index;
            }
        }

        /* Queue the test messages for transmission */
        for (message_index = 0; message_index < num_messages_in_iteration; message_index++)
        {
            const unsigned int selected_index = comms_functions->out_of_order_send_supported ?
                    ((message_index + send_order_offset) % num_messages_in_iteration) :
                    message_index;

            comms_functions->send_message (send_context, send_buffers[selected_index]);
        }
        send_order_offset++;

        /* Wait for the test messages to be received via the external loopback */
        for (message_index = 0; message_index < num_messages_in_iteration; message_index++)
        {
            receive_buffers[message_index] = comms_functions->await_message (receive_context);
        }

        /* Verify the contents of the received messages */
        for (message_index = 0; message_index < num_messages_in_iteration; message_index++)
        {
            const test_message *const message = receive_buffers[message_index]->message;

            CHECK_ASSERT ((message->header.message_length == data_lengths_bytes[message_index]) &&
                          (message->header.message_id == test_pattern_start_values[message_index]) &&
                          (message->header.source_instance == test_pattern_start_values[message_index]));
            for (test_pattern_index = 0; test_pattern_index < data_lengths_words[message_index]; test_pattern_index++)
            {
                CHECK_ASSERT (message->data[test_pattern_index] == test_pattern_start_values[message_index] + test_pattern_index);
            }
        }

        /* Free the received messages */
        for (message_index = 0; message_index < num_messages_in_iteration; message_index++)
        {
            const unsigned int selected_index = comms_functions->out_of_order_free_supported ?
                    ((message_index + free_order_offset) % num_messages_in_iteration) :
                    message_index;

            comms_functions->free_message (receive_context, receive_buffers[selected_index]);
        }
        free_order_offset++;
    }
}

/**
 * @brief Perform a series of message transfer test using one type of Infiniband communication method
 * @param[in] comms_functions The communication functions used to perform the message transfer tests
 */
static void test_message_transfers (const message_communication_functions *const comms_functions)
{
    infiniband_statistics_collection initialisation_stats;
    infiniband_statistics_collection single_thread_message_test_stats;
    infiniband_statistics_collection multi_threaded_message_test_stats;
    api_send_context send_context;
    api_receive_context receive_context;
    sender_thread_context *const send_thread = create_message_sender_thread (comms_functions);
    receiver_thread_context *const receive_thread = create_message_receiver_thread (comms_functions);
    thread_test_definition thread_test_definitions[THREAD_NUM_TESTS];
    message_thread_results thread_test_results[THREAD_NUM_TESTS];
    int test_index;

    /* Initialise */
    printf ("\nTesting using %s\n", comms_functions->description);
    initialise_message_thread_tests (thread_test_definitions);
    get_infiniband_statistics_before_test (&ibv_statistics_handle, &initialisation_stats);
    comms_functions->initialise (&send_context, &receive_context);
    get_infiniband_statistics_after_test (&ibv_statistics_handle, &initialisation_stats);
    send_thread->send_context = send_context;
    receive_thread->receive_context = receive_context;

    /* Test message transfers with differing number of overlapped messages per test iteration,
     * to exercise the buffer management logic.
     * As these tests are performed in the calling thread context there should be zero voluntary context switches */
    get_infiniband_statistics_before_test (&ibv_statistics_handle, &single_thread_message_test_stats);
    increasing_message_size_test (comms_functions, send_context, receive_context, 1);
    increasing_message_size_test (comms_functions, send_context, receive_context, (NUM_MESSAGE_BUFFERS / 2) + 1);
    increasing_message_size_test (comms_functions, send_context, receive_context, NUM_MESSAGE_BUFFERS);
    get_infiniband_statistics_after_test (&ibv_statistics_handle, &single_thread_message_test_stats);

    /* Test message transfers using a send and receive thread.
     * For each test there will be some voluntary context switches as semaphores are used to synchronise
     * the main, send and receive threads. */
    get_infiniband_statistics_before_test (&ibv_statistics_handle, &multi_threaded_message_test_stats);
    for (test_index = 0; test_index < THREAD_NUM_TESTS; test_index++)
    {
        perform_message_thread_test (send_thread, receive_thread,
                                     &thread_test_definitions[test_index], &thread_test_results[test_index]);
    }
    get_infiniband_statistics_after_test (&ibv_statistics_handle, &multi_threaded_message_test_stats);

    /* Free resources */
    display_current_cpu_frequencies ();
    terminate_test_threads (send_thread, receive_thread);
    comms_functions->finalise (send_context, receive_context);

    /* Display results. Left until the end to avoid unnecessary context switches during the test */
    display_infiniband_statistics (&ibv_statistics_handle, &initialisation_stats,
                                   "initialisation of Infiniband communications");
    display_infiniband_statistics (&ibv_statistics_handle, &single_thread_message_test_stats,
                                   "Infiniband single thread message transfers");
    display_infiniband_statistics (&ibv_statistics_handle, &multi_threaded_message_test_stats,
                                   "Infiniband multi-threaded message transfers");
    report_message_thread_test_results (thread_test_results);
}

int main (int argc, char *argv[])
{
    int rc;
    message_communication_functions comms_functions;

    /* Add protection against fork() being called */
    rc = ibv_fork_init ();
    if (rc != 0)
    {
       perror ("ibv_fork_init failed");
       exit (EXIT_FAILURE);
    }

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    open_infiniband_loopback_ports ();

    /* Perform tests with different message passing implementations, in terms of the Infiniband opcodes used */
    sender_rdma_write_receiver_passive_set_functions (&comms_functions);
    test_message_transfers (&comms_functions);
    sender_rdma_write_with_imm_receiver_poll_cq_set_functions (&comms_functions);
    test_message_transfers (&comms_functions);
    sender_send_receiver_recv_set_functions (&comms_functions);
    test_message_transfers (&comms_functions);

    close_ininiband_loopback_ports ();

    return EXIT_SUCCESS;
}
