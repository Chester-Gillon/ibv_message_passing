/*
 * @file ibv_message_bw_main.c
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Program to test sending messages with flow-control from a source Infiniband port to a destination Infiniband port
 */

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <limits.h>
#include <pthread.h>
#include <time.h>
#include <sched.h>
#include <errno.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"

/** The maximum number of message transmit or receive threads */
#define MAX_THREADS 16

/** Delimiter for comma-separated command line arguments */
#define DELIMITER ","

/** Command line arguments which specifies:
 *  - The number of threads
 *  - If each thread transmits or receives messages
 *  - Which communication path instance each thread transmits or receives on */
static unsigned int num_threads;
static bool arg_is_tx_threads[MAX_THREADS];
static int arg_path_instances[MAX_THREADS];

/** Command line arguments which specify the Infiniband device and port used for each thread */
static unsigned int num_ib_devices;
static char *arg_ib_devices[MAX_THREADS];

static unsigned int num_ib_ports;
static uint32_t arg_ib_ports[MAX_THREADS];

/** Optional command line argument which sets which CPU core each transmit or receive thread has its affinity set to */
static int num_cores;
static int arg_cores[MAX_THREADS];

/** Optional command line argument which specifies the maximum message data size which is configured on the communication path */
#define DEFAULT_MAX_MESSAGE_SIZE_BYTES (1024U * 1024U)
static uint32_t arg_max_message_size = DEFAULT_MAX_MESSAGE_SIZE_BYTES;

/** Optional command line option which specifies how many message buffers are configured on the communication path */
#define DEFAULT_NUM_MESSAGE_BUFFERS 16U
static uint32_t arg_num_message_buffers = DEFAULT_NUM_MESSAGE_BUFFERS;

/** Optional command line argument which specifies how buffers are allocated for the transmit and receive messages */
static buffer_allocation_type arg_buffer_allocation_type = BUFFER_ALLOCATION_SHARED_MEMORY;

/** Command line argument which specifies if the data messages have a test pattern which is checked on receipt */
static int arg_verify_data = false;

/** Command line argument which specified if the transmitter polls for Infiniband errors */
static int arg_tx_error_poll = false;

/** Command line argument which controls if the transmitter checks the memory buffer size on the receiver matches */
static int arg_tx_checks_memory_buffer_size = true;

/** Optional command line argument which specifies the min and max message sizes sent */
static uint32_t arg_min_msg_size_sent;
static uint32_t arg_max_msg_size_sent;
static bool arg_min_max_msg_sizes_present = false;

/** The different message IDs used in the test */
typedef enum
{
    /** Header only message which is the first message sent on every buffer.
     *  The transmitter waits for these to be freed to indicate the receive application is running before
     *  starting the message rate tests. */
    TEST_MESSAGE_WARMUP,
    /** Variable size message where the receiver doesn't verify the content */
    TEST_MESSAGE_UNVERIFIED_DATA,
    /** Variable size message where the receiver verifies the content contains a test pattern */
    TEST_MESSAGE_VERIFY_DATA,
    /** Header only message which tells the receiver the test is complete */
    TEST_MESSAGE_TEST_COMPLETE
} test_message_id;

/** Used to build the test results for a message transmit or receive thread */
typedef struct
{
    /** Set true once the communication path is connected, which means messages should start being exchanged */
    bool communication_path_connected;
    /** True if the data content of the message was verified during the test */
    bool verified_data;
    /** The total number of messages processed */
    uint64_t total_messages;
    /** The total number of data bytes in messages processed (excludes the header for each message) */
    uint64_t total_data_bytes;
    /** The extent of the different message lengths seen during the test */
    uint32_t min_message_length_seen;
    uint32_t max_message_length_seen;
    /** The time the first message was transmitted or received */
    struct timespec start_time;
    /** The time the last message was transmitted or received */
    struct timespec stop_time;
    /** The resource usage for the thread before start to transmit or receive messages */
    struct rusage start_usage;
    /** The resource usage for the thread after have completed the transmission of reception of messages */
    struct rusage stop_usage;
} message_test_results;

/** Controls the mode for a thread which transmit test messages */
typedef enum
{
    /** Transmit messages of the maximum message data size configured on the communication path until
     *  requested to stop by the user. */
    CONTINUOS_MAX_MESSAGE_SIZE_SEND,
    /** Transmit messages with a pseudo random data size until requested to stop by the user. */
    CONTINUOS_VARIABLE_SIZE_SEND
} message_transmit_mode;

/** The context for a thread which transmits test messages */
typedef struct
{
    /** Controls how the thread transmits messages */
    message_transmit_mode transmit_mode;
    /** The definition of the communication path for which this thread transmits messages on */
    communication_path_definition path_def;
    /** Handle used for transmitting messages */
    tx_message_context_handle tx_handle;
    /** The results for this thread */
    message_test_results results;
    /** The next generated test pattern word in the message data, initialised to the instance number of the communication path */
    uint32_t test_pattern;
    /** If true this threads inserts a test pattern in the transmitted message data, which the receiver will check */
    bool verify_data;
    /** For CONTINUOS_VARIABLE_SIZE_SEND the data length of each message is a pseudo-random value between these limits */
    uint32_t min_message_send_size;
    uint32_t max_message_send_size;
} message_transmit_thread_context;

/** The context for a thread which receives test messages */
typedef struct
{
    /** The definition of the communication path for which this thread receives messages on */
    communication_path_definition path_def;
    /** Handle used for receiving messages */
    rx_message_context_handle rx_handle;
    /** The results for this thread */
    message_test_results results;
    /** The next expected test pattern word in the message data, initialised to the instance number of the communication path */
    uint32_t test_pattern;
} message_receive_thread_context;

/** The context of one thread which is either a message transmit or receive thread */
typedef struct
{
    /** The identity of the thread */
    pthread_t thread_id;
    /** One of these will be non-NULL to indicate if this is a transmit or receive thread */
    message_transmit_thread_context *tx_thread_context;
    message_receive_thread_context *rx_thread_context;
    /** Set true when the tx/rx thread has been joined. Used by the main thread to report regular status
     *  while at least one thread is running. */
    bool thread_joined;
    /** Used in the main thread to sample the results of the transmit or receive thread,
     *  for reporting message throughput at regular intervals */
    message_test_results previous_results;
    message_test_results current_results;
    /** The CLOCK_MONOTONIC time at which previous_results and current_results were sampled */
    struct timespec previous_results_time;
    struct timespec current_results_time;
} transmit_or_receive_context;

/** The contexts of all message transmit or receive threads which are running in this process */
static transmit_or_receive_context thread_contexts[MAX_THREADS];

/** Set from a signal handler to request that the transmit threads in this process:
 *  - Stop transmitting new data messages.
 *  - Send a message to the receiver that the test is complete, which causes the receive thread to exit.
 *  - The transmit thread to exit once the receiver has freed all the messages which have been sent.
 */
static volatile bool stop_transmission;

/** The command line options for this program, in the format passed to getopt_long().
 *  Only long arguments are supported */
static const struct option command_line_options[] =
{
    {"thread", required_argument, NULL, 0},
    {"ib-dev", required_argument, NULL, 0},
    {"ib-port", required_argument, NULL, 0},
    {"core", required_argument, NULL, 0},
    {"max-msg-size", required_argument, NULL, 0},
    {"num-buffers", required_argument, NULL, 0},
    {"alloc", required_argument, NULL, 0},
    {"verify-data", no_argument, &arg_verify_data, true},
    {"tx-error-poll", no_argument, &arg_tx_error_poll, true},
    {"no-mb-size-check", no_argument, &arg_tx_checks_memory_buffer_size, false},
    {"msg-sizes", required_argument, NULL, 0},
    {NULL, 0, NULL, 0}
};

/**
 * @brief Signal handler to request transmission of messages is stopped
 */
static void stop_transmission_handler (const int sig)
{
    stop_transmission = true;
}

/**
 * @brief Display the usage for this program, and the exit
 */
static void display_usage (void)
{
    printf ("Usage:\n");
    printf ("  ibv_message_bw <options>   Test sending messages with flow-control\n");
    printf ("\n");
    printf ("Options which have one or more comma-separated values which allow one or more\n");
    printf ("message passing threads to be run:\n");
    printf ("  --thread=rx:<instance>|tx:<instance>\n");
    printf ("             Receive or transmit messages on the path with numeric <instance>\n");
    printf ("  --ib-dev=<dev>    Use IB device <dev>\n");
    printf ("  --ib-port=<port>  Use <port> of IB device\n");
    printf ("  --core=<core>     Set the affinity of the tx / rx thread to the CPU <core>\n");
    printf ("\n");
    printf ("Options which apply to all threads, having a single value:\n");
    printf ("  --max-msg-size=<size>  The maximum message data size configured on the path.\n"
            "                         (default %u)\n", DEFAULT_MAX_MESSAGE_SIZE_BYTES);
    printf ("  --num-buffers=<buffers>  The number of message buffers on the path\n"
            "                           (default %u)\n", DEFAULT_NUM_MESSAGE_BUFFERS);
    printf ("  --alloc=heap|shared_mem  How message buffers are allocated\n"
            "                           (default shared_mem)\n");
    printf ("  --verify-data         If specified the transmitter inserts a test pattern\n");
    printf ("                        in the message data which is checked by the receiver.\n");
    printf ("  --tx-error-poll       If specified the transmitter polls for Infiniband\n");
    printf ("                        errors while waiting to obtain a message buffer.\n");
    printf ("  --no-mb-size-check    Disable the check that the message transmitter and\n");
    printf ("                        receiver have the same memory buffer size.\n");
    printf ("  --msg-sizes=<min>,<max>  Send messages with a pseudo-random data size between\n");
    printf ("                           <min> and <max>. Default is all messages sent are the\n");
    printf ("                           maximum message data size configured on the path.\n");
    exit (EXIT_FAILURE);
}

/**
 * @brief Process the thread command line argument which is a comma-separated list
 * @param[in] optdef Option definition
 */
static void process_thread_argument (const struct option *const optdef)
{
    char junk;
    char *arg_copy;
    char *saveptr;
    char *token;
    int instance;
    bool is_tx_thread;

    arg_copy = strdup (optarg);
    token = strtok_r (arg_copy, DELIMITER, &saveptr);
    while (token != NULL)
    {
        if (sscanf (token, "tx:%d%c", &instance, &junk) == 1)
        {
            is_tx_thread = true;
        }
        else if (sscanf (token, "rx:%d%c", &instance, &junk) == 1)
        {
            is_tx_thread = false;
        }
        else
        {
            fprintf (stderr, "Invalid %s instance %s\n", optdef->name, token);
            exit (EXIT_FAILURE);
        }
        if (num_threads < MAX_THREADS)
        {
            arg_is_tx_threads[num_threads] = is_tx_thread;
            arg_path_instances[num_threads] = instance;
            num_threads++;
        }
        else
        {
            fprintf (stderr, "Too many %s instances\n", optdef->name);
            exit (EXIT_FAILURE);
        }
        token = strtok_r (NULL, DELIMITER, &saveptr);
    }
    free (arg_copy);
}

/**
 * @brief Process the ib-dev command line argument which is a comma-separated list
 * @param[in] optdef Option definition
 */
static void process_ib_device_argument (const struct option *const optdef)
{
    char *arg_copy;
    char *saveptr;
    char *token;

    arg_copy = strdup (optarg);
    token = strtok_r (arg_copy, DELIMITER, &saveptr);
    while (token != NULL)
    {
        if (num_ib_devices < MAX_THREADS)
        {
            arg_ib_devices[num_ib_devices] = strdup (token);
            num_ib_devices++;
        }
        else
        {
            fprintf (stderr, "Too many %s instances\n", optdef->name);
            exit (EXIT_FAILURE);
        }
        token = strtok_r (NULL, DELIMITER, &saveptr);
    }
    free (arg_copy);
}

/**
 * @brief Process the ib-port command line argument which is a comma-separated list
 * @param[in] optdef Option definition
 */
static void process_ib_port_argument (const struct option *const optdef)
{
    char *arg_copy;
    char *saveptr;
    char *token;
    char junk;
    uint32_t ib_port;

    arg_copy = strdup (optarg);
    token = strtok_r (arg_copy, DELIMITER, &saveptr);
    while (token != NULL)
    {
        if ((sscanf (token, "%u%c", &ib_port, &junk) != 1) || (ib_port > 255))
        {
            fprintf (stderr, "Invalid %s %s\n", optdef->name, token);
            exit (EXIT_FAILURE);
        }
        if (num_ib_ports < MAX_THREADS)
        {
            arg_ib_ports[num_ib_ports] = ib_port;
            num_ib_ports++;
        }
        else
        {
            fprintf (stderr, "Too many %s instances\n", optdef->name);
            exit (EXIT_FAILURE);
        }
        token = strtok_r (NULL, DELIMITER, &saveptr);
    }
    free (arg_copy);
}

/**
 * @brief Process the core command line argument which is a comma-separated list
 * @param[in] optdef Option definition
 */
static void process_core_argument (const struct option *const optdef)
{
    const int num_cpus = sysconf (_SC_NPROCESSORS_ONLN);
    char *arg_copy;
    char *saveptr;
    char *token;
    char junk;
    int core;

    arg_copy = strdup (optarg);
    token = strtok_r (arg_copy, DELIMITER, &saveptr);
    while (token != NULL)
    {
        if ((sscanf (token, "%d%c", &core, &junk) != 1) ||
            (core < 0) || (core > num_cpus))
        {
            fprintf (stderr, "Invalid %s %s\n", optdef->name, token);
            exit (EXIT_FAILURE);
        }
        if (num_cores < MAX_THREADS)
        {
            arg_cores[num_cores] = core;
            num_cores++;
        }
        else
        {
            fprintf (stderr, "Too many %s instances\n", optdef->name);
            exit (EXIT_FAILURE);
        }
        token = strtok_r (NULL, DELIMITER, &saveptr);
    }
    free (arg_copy);
}

/**
 * @brief Process the msg-sizes command line argument which is a comma-separated list
 * @param[in] optdef Option definition
 */
static void process_msg_sizes_argument (const struct option *const optdef)
{
    char *arg_copy;
    char *saveptr;
    char *token;
    char junk;
    uint32_t size;
    int size_index;

    size_index = 0;
    arg_copy = strdup (optarg);
    token = strtok_r (arg_copy, DELIMITER, &saveptr);
    while (token != NULL)
    {
        if (sscanf (token, "%u%c", &size, &junk) != 1)
        {
            fprintf (stderr, "Invalid %s %s\n", optdef->name, token);
            exit (EXIT_FAILURE);
        }
        else if (size_index == 0)
        {
            arg_min_msg_size_sent = size;
            size_index++;
        }
        else if (size_index == 1)
        {
            arg_max_msg_size_sent = size;
            arg_min_max_msg_sizes_present = true;
            size_index++;
        }
        else
        {
            fprintf (stderr, "%s must contain a min and max message size\n", optdef->name);
            exit (EXIT_FAILURE);
        }
        token = strtok_r (NULL, DELIMITER, &saveptr);
    }
    free (arg_copy);

    if (!arg_min_max_msg_sizes_present)
    {
        fprintf (stderr, "%s must contain a min and max message size\n", optdef->name);
        exit (EXIT_FAILURE);
    }
    if (arg_min_msg_size_sent > arg_max_msg_size_sent)
    {
        fprintf (stderr, "Min message size sent must be <= max message size sent\n");
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Parse command line arguments, storing the result in global variables
 * @details Aborts the program if invalid arguments
 */
static void parse_command_line_arguments (const int argc, char *argv[])
{
    int opt_status;
    char junk;

    do
    {
        int option_index = 0;

        opt_status = getopt_long (argc, argv, "", command_line_options, &option_index);
        if (opt_status == '?')
        {
            display_usage ();
        }
        else if (opt_status >= 0)
        {
            const struct option *const optdef = &command_line_options[option_index];

            if (optdef->flag != NULL)
            {
                /* Argument just sets a flag */
            }
            else if (strcmp (optdef->name, "thread") == 0)
            {
                process_thread_argument (optdef);
            }
            else if (strcmp (optdef->name, "ib-dev") == 0)
            {
                process_ib_device_argument (optdef);
            }
            else if (strcmp (optdef->name, "ib-port") == 0)
            {
                process_ib_port_argument (optdef);
            }
            else if (strcmp (optdef->name, "core") == 0)
            {
                process_core_argument (optdef);
            }
            else if (strcmp (optdef->name, "msg-sizes") == 0)
            {
                process_msg_sizes_argument (optdef);
            }
            else if (strcmp (optdef->name, "max-msg-size") == 0)
            {
                if (sscanf (optarg, "%u%c", &arg_max_message_size, &junk) != 1)
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else if (strcmp (optdef->name, "num-buffers") == 0)
            {
                if (sscanf (optarg, "%u%c", &arg_num_message_buffers, &junk) != 1)
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else if (strcmp (optdef->name, "alloc") == 0)
            {
                if (strcmp (optarg, "heap") == 0)
                {
                    arg_buffer_allocation_type = BUFFER_ALLOCATION_HEAP;
                }
                else if (strcmp (optarg, "shared_mem") == 0)
                {
                    arg_buffer_allocation_type = BUFFER_ALLOCATION_SHARED_MEMORY;
                }
                else
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else
            {
                /* This is a program error, and shouldn't be triggered by the command line options */
                fprintf (stderr, "Unexpected argument definition %s\n", optdef->name);
                exit (EXIT_FAILURE);
            }
        }
    } while (opt_status != -1);

    if (optind < argc)
    {
        fprintf (stderr, "Unexpected non-argument options\n");
        exit (EXIT_FAILURE);
    }

    if (num_threads == 0)
    {
        fprintf (stderr, "At least one tx or rx thread must be specified\n");
        exit (EXIT_FAILURE);
    }
    if ((num_threads != num_ib_devices) || (num_threads != num_ib_ports))
    {
        fprintf (stderr, "The number of ib-dev and ib-port instances must be the same as the number of threads\n");
        exit (EXIT_FAILURE);
    }
    if ((num_cores != 0) && (num_cores != num_threads))
    {
        fprintf (stderr, "When the core argument is specified the number of instances must be the same as the number of threads\n");
        exit (EXIT_FAILURE);
    }
    if (arg_num_message_buffers == 0)
    {
        fprintf (stderr, "num-buffers must be at least one\n");
        exit (EXIT_FAILURE);
    }
    if (arg_max_msg_size_sent > arg_max_message_size)
    {
        fprintf (stderr, "The range of msg-sizes exceeds the maximum message size configured\n");
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Transmit the test warmup messages for a transmit thread
 * @details When this function returns the remote receive thread has freed the warmup messages,
 *          which means the receive thread is ready to start the message rate tests
 * @param[in,out] thread_context The transmit thread context to send the messages for
 */
static void transmit_message_warmup (message_transmit_thread_context *const thread_context)
{
    uint32_t buffer_index;

    for (buffer_index = 0; buffer_index < thread_context->path_def.num_message_buffers; buffer_index++)
    {
        api_message_buffer *const tx_buffer = get_send_buffer (thread_context->tx_handle);

        tx_buffer->header->message_id = TEST_MESSAGE_WARMUP;
        tx_buffer->header->message_length = 0;
        tx_buffer->header->source_instance = thread_context->path_def.instance;
        send_message (thread_context->tx_handle, tx_buffer);
    }
    await_all_outstanding_messages_freed (thread_context->tx_handle);
}

/**
 * @brief Initialise the test results for a message transmit or receive thread
 * @param[out] results The test results to initialise
 */
static void initialise_test_results (message_test_results *const results)
{
    results->total_messages = 0;
    results->total_data_bytes = 0;
    results->verified_data = false;
    results->communication_path_connected = false;
}

/**
 * @brief Update the message test results using one message which has been transmitted or received
 * @param[in,out] results The test results to update
 * @param[in] header The header of the message to update the results with
 */
static void update_test_results (message_test_results *const results, const message_header *const header)
{
    if (header->message_id == TEST_MESSAGE_VERIFY_DATA)
    {
        results->verified_data = true;
    }

    if (results->total_messages == 0)
    {
        results->min_message_length_seen = header->message_length;
        results->max_message_length_seen = header->message_length;
    }
    else
    {
        if (header->message_length < results->min_message_length_seen)
        {
            results->min_message_length_seen = header->message_length;
        }
        if (header->message_length > results->max_message_length_seen)
        {
            results->max_message_length_seen = header->message_length;
        }
    }

    results->total_messages++;
    results->total_data_bytes += header->message_length;
}

/**
 * @brief Perform a message transmit test for one thread, stopping the test when requested by a signal handler
 * @param[in,out] thread_context The transmit thread context to send the messages for
 */
static void transmit_message_test (message_transmit_thread_context *const thread_context)
{
    api_message_buffer *tx_buffer;
    uint32_t word_index;
    uint32_t num_tx_words;
    uint32_t *tx_words;
    uint32_t base_message_length;
    bool variable_message_length;
    const uint32_t message_length_random_modulo =
            (thread_context->max_message_send_size - thread_context->min_message_send_size) + 1;
    unsigned int seed = thread_context->path_def.instance;

    if (thread_context->transmit_mode == CONTINUOS_VARIABLE_SIZE_SEND)
    {
        variable_message_length = thread_context->min_message_send_size != thread_context->max_message_send_size;
        base_message_length = thread_context->min_message_send_size;
    }
    else
    {
        variable_message_length = false;
        base_message_length = thread_context->path_def.max_message_size;
    }

    clock_gettime (CLOCK_MONOTONIC, &thread_context->results.start_time);
    while (!stop_transmission)
    {
        tx_buffer = get_send_buffer (thread_context->tx_handle);
        tx_words = (uint32_t *) tx_buffer->data;
        tx_buffer->header->message_length = base_message_length;
        if (variable_message_length)
        {
            tx_buffer->header->message_length += rand_r (&seed) % message_length_random_modulo;
        }
        tx_buffer->header->source_instance = thread_context->test_pattern;
        if (thread_context->verify_data)
        {
            tx_buffer->header->message_id = TEST_MESSAGE_VERIFY_DATA;
            num_tx_words = tx_buffer->header->message_length / sizeof (uint32_t);
            for (word_index = 0; word_index < num_tx_words; word_index++)
            {
                tx_words[word_index] = thread_context->test_pattern;
                thread_context->test_pattern++;
            }
        }
        else
        {
            tx_buffer->header->message_id = TEST_MESSAGE_UNVERIFIED_DATA;
        }
        send_message (thread_context->tx_handle, tx_buffer);
        update_test_results (&thread_context->results, tx_buffer->header);
    }
    await_all_outstanding_messages_freed (thread_context->tx_handle);
    clock_gettime (CLOCK_MONOTONIC, &thread_context->results.stop_time);

    tx_buffer = get_send_buffer (thread_context->tx_handle);
    tx_buffer->header->message_id = TEST_MESSAGE_TEST_COMPLETE;
    tx_buffer->header->message_length = 0;
    send_message (thread_context->tx_handle, tx_buffer);
}

/**
 * @details The entry point for a thread which transmits test messages on a communication path.
 *          This transmits messages as quickly as possible until signalled to end the test.
 * @param[in,out] arg The context for the transmit thread.
 *                    On entry defines the communication path to create the transmit endpoint for.
 *                    On exit contains the transmitted message statistics.
 * @return Not used
 */
static void *message_transmit_thread (void *const arg)
{
    message_transmit_thread_context *const thread_context = (message_transmit_thread_context *) arg;
    int rc;

    thread_context->test_pattern = thread_context->path_def.instance;
    thread_context->tx_handle = message_transmit_create_local (&thread_context->path_def);
    message_transmit_attach_remote (thread_context->tx_handle);
    rc = getrusage (RUSAGE_THREAD, &thread_context->results.start_usage);
    check_assert (rc == 0, "getrusage");
    thread_context->results.communication_path_connected = true;
    transmit_message_warmup (thread_context);

    transmit_message_test (thread_context);

    rc = getrusage (RUSAGE_THREAD, &thread_context->results.stop_usage);
    check_assert (rc == 0, "getrusage");
    message_transmit_finalise (thread_context->tx_handle);

    return NULL;
}

/**
 * @brief Receive and check the test warmup messages for a receive thread
 * @param[in,out] thread_context The receive thread context to receive the messages for
 */
static void receive_message_warmup (message_receive_thread_context *const thread_context)
{
    uint32_t buffer_index;

    for (buffer_index = 0; buffer_index < thread_context->path_def.num_message_buffers; buffer_index++)
    {
        api_message_buffer *const rx_buffer = await_message (thread_context->rx_handle);

        CHECK_ASSERT ((rx_buffer->header->message_id == TEST_MESSAGE_WARMUP) &&
                      (rx_buffer->header->message_length == 0) &&
                      (rx_buffer->header->source_instance == thread_context->path_def.instance));
        free_message (thread_context->rx_handle, rx_buffer);
    }
}

/**
 * @brief Perform a message receive test for one thread, exiting when receive the end of message test
 * @param[in,out] thread_context The receive thread context for perform the test for
 */
static void receive_message_test (message_receive_thread_context *const thread_context)
{
    bool test_complete;

    test_complete = false;
    while (!test_complete)
    {
        api_message_buffer *const rx_buffer = await_message (thread_context->rx_handle);

        switch (rx_buffer->header->message_id)
        {
        case TEST_MESSAGE_UNVERIFIED_DATA:
        case TEST_MESSAGE_VERIFY_DATA:
            if (thread_context->results.total_messages == 0)
            {
                clock_gettime (CLOCK_MONOTONIC, &thread_context->results.start_time);
            }
            update_test_results (&thread_context->results, rx_buffer->header);
            if (rx_buffer->header->message_id == TEST_MESSAGE_VERIFY_DATA)
            {
                /* Verify that the received message contains the expected test pattern */
                const uint32_t num_rx_words = rx_buffer->header->message_length / sizeof (uint32_t);
                const uint32_t *const rx_words = (const uint32_t *) rx_buffer->data;
                uint32_t word_index;

                CHECK_ASSERT (rx_buffer->header->source_instance == thread_context->test_pattern);
                for (word_index = 0; word_index < num_rx_words; word_index++)
                {
                    CHECK_ASSERT (rx_words[word_index] == thread_context->test_pattern);
                    thread_context->test_pattern++;
                }
            }
            break;

        case TEST_MESSAGE_TEST_COMPLETE:
            clock_gettime (CLOCK_MONOTONIC, &thread_context->results.stop_time);
            test_complete = true;
            break;

        default:
            check_assert (false, "receive_message_test unknown message");
            break;
        }
        free_message (thread_context->rx_handle, rx_buffer);
    }
}

/**
 * @brief The entry point for a thread which receives and frees test messages on a communication path.
 *        This will exit when indicated by the received message type.
 * @param[in,out] arg The context for the receive thread.
 *                    On entry defines the communication path to create the receive endpoint for.
 *                    On exit contains the received message statistics.
 * @return Not used
 */
static void *message_receive_thread (void *const arg)
{
    message_receive_thread_context *const thread_context = (message_receive_thread_context *) arg;
    int rc;

    thread_context->test_pattern = thread_context->path_def.instance;
    thread_context->rx_handle = message_receive_create_local (&thread_context->path_def);
    message_receive_attach_remote (thread_context->rx_handle);
    rc = getrusage (RUSAGE_THREAD, &thread_context->results.start_usage);
    check_assert (rc == 0, "getrusage");
    thread_context->results.communication_path_connected = true;
    receive_message_warmup (thread_context);

    receive_message_test (thread_context);
    rc = getrusage (RUSAGE_THREAD, &thread_context->results.stop_usage);
    check_assert (rc == 0, "getrusage");

    message_receive_finalise (thread_context->rx_handle);

    return NULL;
}

/**
 * @brief Create all the message transmit or receive threads specified by the command line options.
 */
static void create_message_threads (void)
{
    communication_path_definition path_def;
    pthread_attr_t thread_attr;
    int rc;
    uint32_t thread_index;
    unsigned int num_tx_threads;

    /* Create the threads */
    num_tx_threads = 0;
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        transmit_or_receive_context *const tx_or_rx = &thread_contexts[thread_index];

        /* Set the path definition which is common to the message transmitter and receiver */
        path_def.ib_device = arg_ib_devices[thread_index];
        path_def.port_num = (uint8_t) arg_ib_ports[thread_index];
        path_def.instance = arg_path_instances[thread_index];
        path_def.max_message_size = arg_max_message_size;
        path_def.num_message_buffers = arg_num_message_buffers;
        path_def.allocation_type = arg_buffer_allocation_type;
        path_def.tx_polls_for_errors = arg_tx_error_poll;
        path_def.tx_checks_memory_buffer_size = arg_tx_checks_memory_buffer_size;

        /* Set CPU affinity for the thread which sends or receives messages, if specified as a command line option */
        rc = pthread_attr_init (&thread_attr);
        check_assert (rc == 0, "pthread_attr_init");
        if (num_cores > 0)
        {
            cpu_set_t cpuset;

            CPU_ZERO (&cpuset);
            CPU_SET (arg_cores[thread_index], &cpuset);
            rc = pthread_attr_setaffinity_np (&thread_attr, sizeof (cpuset), &cpuset);
            check_assert (rc == 0, "pthread_attr_setaffinty_np");
        }

        /* Create either transmit or receive thread */
        if (arg_is_tx_threads[thread_index])
        {
            tx_or_rx->tx_thread_context = cache_line_aligned_calloc (1, sizeof (message_transmit_thread_context));
            tx_or_rx->rx_thread_context = NULL;
            tx_or_rx->thread_joined = false;
            tx_or_rx->tx_thread_context->path_def = path_def;
            tx_or_rx->tx_thread_context->verify_data = arg_verify_data;
            if (arg_min_max_msg_sizes_present)
            {
                tx_or_rx->tx_thread_context->transmit_mode = CONTINUOS_VARIABLE_SIZE_SEND;
                tx_or_rx->tx_thread_context->min_message_send_size = arg_min_msg_size_sent;
                tx_or_rx->tx_thread_context->max_message_send_size = arg_max_msg_size_sent;
            }
            else
            {
                tx_or_rx->tx_thread_context->transmit_mode = CONTINUOS_MAX_MESSAGE_SIZE_SEND;
            }
            initialise_test_results (&tx_or_rx->tx_thread_context->results);
            tx_or_rx->previous_results = tx_or_rx->tx_thread_context->results;
            clock_gettime (CLOCK_MONOTONIC, &tx_or_rx->current_results_time);
            rc = pthread_create (&tx_or_rx->thread_id, &thread_attr, message_transmit_thread, tx_or_rx->tx_thread_context);
            check_assert (rc == 0, "pthread_create");
            num_tx_threads++;
        }
        else
        {
            tx_or_rx->tx_thread_context = NULL;
            tx_or_rx->rx_thread_context = cache_line_aligned_calloc (1, sizeof (message_receive_thread_context));
            tx_or_rx->thread_joined = false;
            tx_or_rx->rx_thread_context->path_def = path_def;
            initialise_test_results (&tx_or_rx->rx_thread_context->results);
            tx_or_rx->previous_results = tx_or_rx->rx_thread_context->results;
            clock_gettime (CLOCK_MONOTONIC, &tx_or_rx->current_results_time);
            rc = pthread_create (&tx_or_rx->thread_id, &thread_attr, message_receive_thread, tx_or_rx->rx_thread_context);
            check_assert (rc == 0, "pthread_create");
        }

        rc = pthread_attr_destroy (&thread_attr);
        check_assert (rc == 0, "pthread_attr_destroy");
    }

    if (num_tx_threads > 0)
    {
        /* Install a signal handler to allow a request to stop transmission */
        struct sigaction action;

        printf ("Press Ctrl-C to tell the %u transmit thread(s) to stop the test\n", num_tx_threads);
        memset (&action, 0, sizeof (action));
        action.sa_handler = stop_transmission_handler;
        action.sa_flags = SA_RESTART;
        rc = sigaction (SIGINT, &action, NULL);
        check_assert (rc == 0, "sigaction");
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
 * @brief Report the progress for one message transmit or receive thread during a test
 * @param[in] thread_index Which thread to report the progress for
 */
static void report_message_thread_regular_progress (const uint32_t thread_index)
{
    transmit_or_receive_context *const tx_or_rx = &thread_contexts[thread_index];
    const char *const direction = arg_is_tx_threads[thread_index] ? "Tx" : "Rx";

    /* Sample the current results */
    tx_or_rx->previous_results = tx_or_rx->current_results;
    tx_or_rx->previous_results_time = tx_or_rx->current_results_time;
    if (arg_is_tx_threads[thread_index])
    {
        tx_or_rx->current_results = tx_or_rx->tx_thread_context->results;
    }
    else
    {
        tx_or_rx->current_results = tx_or_rx->rx_thread_context->results;
    }
    clock_gettime (CLOCK_MONOTONIC, &tx_or_rx->current_results_time);

    /* Report the results */
    if (tx_or_rx->current_results.communication_path_connected && !tx_or_rx->previous_results.communication_path_connected)
    {
        printf ("%s_%d connected\n", direction, arg_path_instances[thread_index]);
    }

    if (tx_or_rx->current_results.total_messages > 0)
    {
        const double report_interval_secs =
                get_elapsed_ns (&tx_or_rx->previous_results_time, &tx_or_rx->current_results_time) / 1E9;

        printf ("%s_%d %s %lu data bytes in %lu messages over last %.1f seconds\n",
                direction, arg_path_instances[thread_index],
                arg_is_tx_threads[thread_index] ? "transmitted" : "received",
                        tx_or_rx->current_results.total_data_bytes - tx_or_rx->previous_results.total_data_bytes,
                tx_or_rx->current_results.total_messages - tx_or_rx->previous_results.total_messages,
                report_interval_secs);
    }
}

/**
 * @brief Wait for all the message transmit or receive threads in this process to exit.
 * @details While waiting display regular progress of the message tests
 * @todo Since CLOCK_REALTIME is used as the time source for reporting regular progress
 *       the reporting of progress will stall if the real time jumps backwards.
 */
static void wait_for_message_threads_to_exit (void)
{
    const int report_interval_seconds = 10;
    uint32_t thread_index;
    int rc;
    void *thread_result;
    struct timespec abs_time;
    uint32_t num_threads_running;

    num_threads_running = num_threads;
    clock_gettime (CLOCK_REALTIME, &abs_time);

    /* While any thread is still running try and join a running thread.
     * Where the timeout is used to trigger the reporting of progress. */
    while (num_threads_running > 0)
    {
        thread_index = 0;
        while ((thread_index < num_threads) && (thread_contexts[thread_index].thread_joined))
        {
            thread_index++;
        }
        rc = pthread_timedjoin_np (thread_contexts[thread_index].thread_id, &thread_result, &abs_time);
        if (rc == 0)
        {
            CHECK_ASSERT ((num_threads_running > 0) && !thread_contexts[thread_index].thread_joined);
            thread_contexts[thread_index].thread_joined = true;
            num_threads_running--;
        }
        else if (rc == ETIMEDOUT)
        {
            /* Report progress for each thread */
            for (thread_index = 0; thread_index < num_threads; thread_index++)
            {
                report_message_thread_regular_progress (thread_index);
            }
            abs_time.tv_sec += report_interval_seconds;
        }
        else
        {
            check_assert (false, "pthread_timedjoin_np");
        }
    }
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
 * @details Display the final results for one message transmit of receive thread which is:
 *          - The total number of messages and data bytes
 *          - The Linux usage measurements
 * @param[in] thread_index Which thread to display the results for
 */
static void display_message_thread_final_results (const uint32_t thread_index)
{
    transmit_or_receive_context *const tx_or_rx = &thread_contexts[thread_index];
    const char *const direction = arg_is_tx_threads[thread_index] ? "Tx" : "Rx";
    const message_test_results *const results = arg_is_tx_threads[thread_index] ?
            &tx_or_rx->tx_thread_context->results : &tx_or_rx->rx_thread_context->results;
    const double test_duration_secs = (double) get_elapsed_ns (&results->start_time, &results->stop_time) / 1E9;

    printf ("\n");
    printf ("%s_%d Total data bytes %lu over %.6f seconds; %.1f Mbytes/second\n",
            direction, arg_path_instances[thread_index],
            results->total_data_bytes, test_duration_secs,
            ((double) results->total_data_bytes / test_duration_secs) / 1E6);
    printf ("%s_%d Total messages %lu over %.6f seconds; %.0f messages/second\n",
            direction, arg_path_instances[thread_index],
            results->total_messages, test_duration_secs,
            (double) results->total_messages / test_duration_secs);
    printf ("%s_%d Min message size=%u max message size=%u data verification=%s\n",
            direction, arg_path_instances[thread_index],
            results->min_message_length_seen, results->max_message_length_seen,
            results->verified_data ? "yes" : "no");
    printf ("%s_%d minor page faults=%ld (%ld -> %ld)\n",
            direction, arg_path_instances[thread_index],
            results->stop_usage.ru_minflt - results->start_usage.ru_minflt,
            results->start_usage.ru_minflt, results->stop_usage.ru_minflt);
    printf ("%s_%d major page faults=%ld (%ld -> %ld)\n",
            direction, arg_path_instances[thread_index],
            results->stop_usage.ru_majflt - results->start_usage.ru_majflt,
            results->start_usage.ru_majflt, results->stop_usage.ru_majflt);
    printf ("%s_%d voluntary context switches=%ld (%ld -> %ld)\n",
            direction, arg_path_instances[thread_index],
            results->stop_usage.ru_nvcsw - results->start_usage.ru_nvcsw,
            results->start_usage.ru_nvcsw, results->stop_usage.ru_nvcsw);
    printf ("%s_%d involuntary context switches=%ld (%ld -> %ld)\n",
            direction, arg_path_instances[thread_index],
            results->stop_usage.ru_nivcsw - results->start_usage.ru_nivcsw,
            results->start_usage.ru_nivcsw, results->stop_usage.ru_nivcsw);
    printf ("%s_%d user time=%.6f system time=%.6f\n",
            direction, arg_path_instances[thread_index],
            rusage_utilisation_secs (&results->start_usage.ru_utime, &results->stop_usage.ru_utime),
            rusage_utilisation_secs (&results->start_usage.ru_stime, &results->stop_usage.ru_stime));
}

int main (int argc, char *argv[])
{
    uint32_t thread_index;
    SLPError slp_status;
    SLPHandle slp_main_thread_handle;

    parse_command_line_arguments (argc, argv);

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    /* @todo The first SLPOpen call reads configuration parameters from the SLP configuration file into a linked list,
     *       which is freed when the last handle is freed.
     *       When SLPOpen was initially only called in each message thread, SLPOpen would sometimes cause a SIGSEGV
     *       due to the information read from the SLP configuration file being in an inconsistent state.
     *       As a work-around call SLPOpen to get a dummy handle and read the SLP configuration file in the main
     *       thread before SLPOpen is used in each message thread. */
    slp_status = SLPOpen (NULL, SLP_FALSE, &slp_main_thread_handle);
    check_assert (slp_status == SLP_OK, "SLPOpen");

    create_message_threads ();
    wait_for_message_threads_to_exit ();
    for (thread_index = 0; thread_index < num_threads; thread_index++)
    {
        display_message_thread_final_results (thread_index);
    }

    SLPClose (slp_main_thread_handle);

    return EXIT_SUCCESS;
}
