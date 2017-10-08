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

#include <infiniband/verbs.h>

#include "ibv_message_bw_interface.h"

/** Command line arguments which specify which communication path instance this program is the source or destination for.
 *  One and only one of these options should be present. */
static int arg_rx_instance;
static bool arg_rx_instance_present = false;

static int arg_tx_instance;
static bool arg_tx_instance_present = false;

/** Command line argument which specifies the Infiniband device and port to send or receive messages on */
static char arg_ib_device[NAME_MAX];

static uint32_t arg_ib_port;
static bool arg_ib_port_present = false;

/** Optional command line argument which specifies the maximum message data size which is configured on the communication path */
#define DEFAULT_MAX_MESSAGE_SIZE_BYTES (1024U * 1024U)
static uint32_t arg_max_message_size = DEFAULT_MAX_MESSAGE_SIZE_BYTES;

/** Optional command line option which specifies how many message buffers are configured on the communication path */
#define DEFAULT_NUM_MESSAGE_BUFFERS 16U
static uint32_t arg_num_message_buffers = DEFAULT_NUM_MESSAGE_BUFFERS;

/** Optional command line argument which sets which CPU core the transmit or receive has its affinity set to */
static int arg_core;
static bool arg_core_present = false;

/** The context for a thread which transmits test messages */
typedef struct
{
    /** The identity of the thread */
    pthread_t thread_id;
    /** The definition of the communication path for which this thread transmit messages on */
    communication_path_definition path_def;
} message_transmit_thread_context;

/** The context for a thread which receives test messages */
typedef struct
{
    /** The identity of the thread */
    pthread_t thread_id;
    /** The definition of the communication path for which this thread receives messages on */
    communication_path_definition path_def;
} message_receive_thread_context;

/** The command line options for this program, in the format passed to getopt_long().
 *  Only long arguments are supported */
static const struct option command_line_options[] =
{
    {"rx", required_argument, NULL, 0},
    {"tx", required_argument, NULL, 0},
    {"ib-dev", required_argument, NULL, 0},
    {"ib-port", required_argument, NULL, 0},
    {"max-msg-size", required_argument, NULL, 0},
    {"num-buffers", required_argument, NULL, 0},
    {"core", required_argument, NULL, 0},
    {NULL, 0, NULL, 0}
};

/**
 * @brief Display the usage for this program, and the exit
 */
static void display_usage (void)
{
    printf ("Usage:\n");
    printf ("  ibv_message_bw <options>   Test sending messages with flow-control\n");
    printf ("\n");
    printf ("Options:\n");
    printf ("  --rx=<instance>        Receive messages on the path with numeric <instance>\n");
    printf ("  --tx=<instance>        Transmit messages on the path with numeric <instance>\n");
    printf ("  --ib-dev=<dev>         Use IB device <dev>\n");
    printf ("  --ib-port=<port>       Use <port> of IB device\n");
    printf ("  --max-msg-size=<size>  The maximum message data size configured on the path.\n"
            "                         (default %u)\n", DEFAULT_MAX_MESSAGE_SIZE_BYTES);
    printf ("  --num-buffers=<buffers>  The number of message buffers on the path\n"
            "                           (default %u)\n", DEFAULT_NUM_MESSAGE_BUFFERS);
    printf ("  --core=<core>     Set the affinity of the tx / rx thread to the CPU <core>\n");
    exit (EXIT_FAILURE);
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
            else if (strcmp (optdef->name, "rx") == 0)
            {
                if (sscanf (optarg, "%d%c", &arg_rx_instance, &junk) != 1)
                {
                    fprintf (stderr, "Invalid %s instance %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
                arg_rx_instance_present = true;
            }
            else if (strcmp (optdef->name, "tx") == 0)
            {
                if (sscanf (optarg, "%d%c", &arg_tx_instance, &junk) != 1)
                {
                    fprintf (stderr, "Invalid %s instance %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
                arg_tx_instance_present = true;
            }
            else if (strcmp (optdef->name, "ib-dev") == 0)
            {
                strcpy (arg_ib_device, optarg);
            }
            else if (strcmp (optdef->name, "ib-port") == 0)
            {
                if ((sscanf (optarg, "%u%c", &arg_ib_port, &junk) != 1) || (arg_ib_port > 255))
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
                arg_ib_port_present = true;
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
            else if (strcmp (optdef->name, "core") == 0)
            {
                const int num_cpus = sysconf (_SC_NPROCESSORS_ONLN);

                if ((sscanf (optarg, "%d%c", &arg_core, &junk) != 1) ||
                    (arg_core < 0) || (arg_core > num_cpus))
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
                arg_core_present = true;
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

    if (!(arg_tx_instance_present ^ arg_rx_instance_present))
    {
        fprintf (stderr, "One and only one of --tx or --rx must be specified\n");
        exit (EXIT_FAILURE);
    }
    if ((strlen (arg_ib_device) == 0) || !arg_ib_port_present)
    {
        fprintf (stderr, "The Infiniband device and port must be specified\n");
        exit (EXIT_FAILURE);
    }
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
    tx_message_context_handle tx_handle;

    tx_handle = message_transmit_create_local (&thread_context->path_def);
    message_transmit_attach_remote (tx_handle);

    message_transmit_finalise (tx_handle);

    return NULL;
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
    rx_message_context_handle rx_handle;

    rx_handle = message_receive_create_local (&thread_context->path_def);
    message_receive_attach_remote (rx_handle);

    message_receive_finalise (rx_handle);

    return NULL;
}

/**
 * @details Sequence performing a message transmit test by:
 *          - Creating a thread to transmit messages on a communication path.
 *          - Waiting for the transmit thread to exit, when the test is complete.
 *            While waiting displays the throughput of the transmitted messages at regular intervals.
 *          - Display the final transmitted message statistics.
 * @param[in] path_def The definition of the communication path to receive messages from
 * @param[in] thread_attr The attributes for the receive thread
 */
static void perform_message_transmit_test (const communication_path_definition *const path_def,
                                           pthread_attr_t *const thread_attr)
{
    message_transmit_thread_context *const thread_context =
            cache_line_aligned_calloc (1, sizeof (message_transmit_thread_context));
    int rc;
    void *thread_result;
    struct timespec abs_time;
    bool test_complete;

    /* Start the message transmit thread */
    thread_context->path_def = *path_def;
    rc = pthread_create (&thread_context->thread_id, thread_attr, message_transmit_thread, thread_context);
    check_assert (rc == 0, "pthread_create");

    /* Wait for the transmit thread to exit, reporting throughput at regular intervals */
    clock_gettime (CLOCK_REALTIME, &abs_time);
    do
    {
        abs_time.tv_sec += 2;
        rc = pthread_timedjoin_np (thread_context->thread_id, &thread_result, &abs_time);
        if (rc == 0)
        {
            test_complete = true;
        }
        else if (rc == ETIMEDOUT)
        {
            test_complete = false;
        }
        else
        {
            check_assert (false, "pthread_timedjoin_np");
        }
    } while (!test_complete);

    free (thread_context);
}

/**
 * @details Sequence performing a message receive test by:
 *          - Creating a thread to receive messages on a communication path.
 *          - Waiting for the receive thread to exit, when the test is complete.
 *          - Display the received message statistics.
 * @param[in] path_def The definition of the communication path to receive messages from
 * @param[in] thread_attr The attributes for the receive thread
 */
static void perform_message_receive_test (const communication_path_definition *const path_def,
                                          pthread_attr_t *const thread_attr)
{
    message_receive_thread_context *const thread_context =
            cache_line_aligned_calloc (1, sizeof (message_receive_thread_context));
    int rc;
    void *thread_result;

    /* Start the message receive thread */
    thread_context->path_def = *path_def;
    rc = pthread_create (&thread_context->thread_id, thread_attr, message_receive_thread, thread_context);
    check_assert (rc == 0, "pthread_create");

    /* Wait for the receive thread to exit */
    rc = pthread_join (thread_context->thread_id, &thread_result);
    check_assert (rc == 0, "pthread_join");

    free (thread_context);
}

int main (int argc, char *argv[])
{
    communication_path_definition path_def;
    pthread_attr_t thread_attr;
    int rc;

    parse_command_line_arguments (argc, argv);

    /* Set the path definition which is common to the message transmitter and receiver */
    path_def.ib_device = arg_ib_device;
    path_def.port_num = (uint8_t) arg_ib_port;
    path_def.instance = arg_rx_instance_present ? arg_rx_instance : arg_tx_instance;
    path_def.max_message_size = arg_max_message_size;
    path_def.num_message_buffers = arg_num_message_buffers;

    /* Set CPU affinity for the thread which sends or receives messages, if specified as a command line option */
    rc = pthread_attr_init (&thread_attr);
    check_assert (rc == 0, "pthread_attr_init");
    if (arg_core_present)
    {
        cpu_set_t cpuset;

        CPU_ZERO (&cpuset);
        CPU_SET (arg_core, &cpuset);
        rc = pthread_attr_setaffinity_np (&thread_attr, sizeof (cpuset), &cpuset);
        check_assert (rc == 0, "pthread_attr_setaffinty_np");
    }

    /* Perform the message transmit or receive test, according to the command line options */
    if (arg_tx_instance_present)
    {
        perform_message_transmit_test (&path_def, &thread_attr);
    }
    else
    {
        perform_message_receive_test (&path_def, &thread_attr);
    }
    rc = pthread_attr_destroy (&thread_attr);
    check_assert (rc == 0, "pthread_attr_destroy");

    return EXIT_SUCCESS;
}
