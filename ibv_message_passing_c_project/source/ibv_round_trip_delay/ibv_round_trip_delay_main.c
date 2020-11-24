/*
 * @file ibv_round_trip_delay_main.c
 * @date 15 Nov 2020
 * @author Chester Gillon
 * @brief Measure the round-trip delay of exchanging messages by either TCP sockets or RDMA.
 * @details Created to evaluate the per-message overhead of using either TCP sockets or RDMA.
 *          By using the ibv_message_transport library the RDMA can be either over Infiniband, or RoCE over Ethernet.
 *
 *          The test is run as a client and server on two endpoints. It has one or more message threads, where each
 *          thread exchanges messages as quickly as possible with a corresponding thread in the remote endpoint.
 *
 *          The message rate is reported as how many iterations of the following for each thread:
 *             client -> server -> client.
 *
 *          I.e. the rate at which the server can echo a message back to the client.
 *          As there is only one message in flight at once per thread, the one-way message latency is approx (2 / message_rate).
 *
 *          By increasing the number of threads running in parallel can see how the rate scales, and if multiple threads obtain
 *          an equal message rate.
 *
 *          Each thread has its affinity set to a core specified by the command line arguments.
 *
 *          TCP sockets are used as busy-polling by using ioctl() to wait for receipt of sufficient bytes for one message,
 *          analogous to how the ibv_message_transport library polls for message receipt when using RDMA.
 *          There is no attempt to have a hold-off the rate at which ioctl() is used for polling.
 *
 * @todo TCP was used as allow a connect operation. As the program exchanges short messages, using UDP sockets might have
 *       less per-message overheads.
 *
 *       However, since the ibv_message_transport library uses Reliable Connection (RC) Queue-Pairs for RDMA use of TCP
 *       is nominally equivalent.
 */

#define _GNU_SOURCE

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include <errno.h>
#include <unistd.h>
#include <getopt.h>
#include <arpa/inet.h>
#include <limits.h>
#include <linux/mempolicy.h>
#include <numa.h>
#include <numaif.h>
#include <sched.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"

/** The maximum number of message threads */
#define MAX_THREADS 16

/** Delimiter for comma-separated command line arguments */
#define DELIMITER ","

/** Command line argument which specifies which role this program performs. */
typedef enum
{
    TEST_ROLE_RDMA_SERVER,
    TEST_ROLE_RDMA_CLIENT,
    TEST_ROLE_TCP_SERVER,
    TEST_ROLE_TCP_CLIENT
} test_role_t;
static test_role_t arg_test_role;

/** Used to allocate unique nodes for communication paths across all threads tested */
#define SERVER_NODE_OFFSET   0
#define CLIENT_NODE_OFFSET   1
#define NUM_NODES_PER_THREAD 2

/** Message IDs used in test:
 *  - ECHO_MSG_ID sent from client to server, with data size of arg_message_size, which server echos back to client.
 *  - EXIT_MSG_ID sent from client to server to cause the server to exit at the end of the test.
 */
#define ECHO_MSG_ID 0
#define EXIT_MSG_ID 1

/** Command line argument which specify which cores the message threads run on */
static int arg_cores[MAX_THREADS];
static uint32_t arg_num_cores;

/** Command line argument which specifies the Infiniband device and port for RDMA transport */
static char *arg_ib_device;
static uint32_t arg_ib_port;

/** Command line argument which specifies the base server port for TCP transport, incremented for each thread */
static uint32_t arg_tcp_base_port = 52000;

/** Command line argument which specifies the TCP server address, to which the client connects */
static struct in_addr arg_tcp_server_addr;

/** Command line argument which specifies the data size in bytes of the message exchanged */
static uint32_t arg_message_size = 120;

/** Used to make the main thread and all message threads wait until all message threads have initialised before the test
 *  starts measuring the round-trip delay. This is so that all message threads are timed running in parallel. */
static pthread_barrier_t threads_ready_barrier;

/** Contains the context for one message thread when using RDMA transport */
typedef struct
{
    /** The allocated local node number for this thread, to select the communication paths */
    int local_node_number;
    /** The remote node number this thread communicates with */
    int remote_node_number;
    /** Communication context to obtain communication paths for the thread */
    communication_context_handle node_context;
    /** Used to transmit messages to the remote node */
    tx_message_context_handle tx_handle;
} message_thread_rdma_context;

/** Contains the context for one message thread when using TCP transport */
typedef struct
{
    /** The bi-directional data socket used to exchange messages with the remote end */
    int data_socket;
    /** The number of bytes exchanged in each message. A header plus arg_message_size for the data */
    size_t buffer_size_bytes;
    /** Buffer of buffer_size_bytes used to transmit messages */
    uint32_t *tx_buffer;
    /** Buffer of buffer_size_bytes used to receive messages */
    uint32_t *rx_buffer;
} message_thread_tcp_context;

/** Used to build the test results for a message thread */
typedef struct
{
    /** The total number of message exchanged */
    uint64_t total_messages;
    /** The time the first message was transmitted or received */
    struct timespec start_time;
    /** The time the last message was transmitted or received */
    struct timespec stop_time;
    /** The resource usage for the thread before start to transmit or receive messages */
    struct rusage start_usage;
    /** The resource usage for the thread after have completed the transmission of reception of messages */
    struct rusage stop_usage;
} message_round_trip_results;

/** Contains the context for one message thread */
typedef struct
{
    /** The identity of the thread */
    pthread_t thread_id;
    /** Set true when the message thread has been joined. Used by the main thread to report regular status
     *  while at least one thread is running. */
    bool thread_joined;
    /** The instance number of thread, in the range 0..arg_num_cores-1. Used to select the RDMA communication paths or
     *  TCP port used. */
    int thread_index;
    /** The NUMA node mask used to set the memory policy to allocate from the NUMA node local to the core on
     *  which the thread has its affinity set to. */
    struct bitmask *numa_mask;
    /** Context used for RDMA transport */
    message_thread_rdma_context rdma;
    /** Context used for TCP transport */
    message_thread_tcp_context tcp;
    /** Set once an EXIT_MSG_ID has been exchanged to indicate the test is complete, and the thread exits */
    bool test_complete;
    /** The results for this thread */
    message_round_trip_results results;
    /** Used in the main thread to sample the results of the transmit or receive thread,
     *  for reporting message throughput at regular intervals */
    message_round_trip_results previous_results;
    message_round_trip_results current_results;
    /** The CLOCK_MONOTONIC time at which previous_results and current_results were sampled */
    struct timespec previous_results_time;
    struct timespec current_results_time;
} message_thread_context;

/** The contexts of all message threads which are running in this process */
static message_thread_context thread_contexts[MAX_THREADS];


/** The command line options for this program, in the format passed to getopt_long().
 *  Only long arguments are supported */
static const struct option command_line_options[] =
{
    {"role", required_argument, NULL, 0},
    {"cores", required_argument, NULL, 0},
    {"ib-dev", required_argument, NULL, 0},
    {"ib-port", required_argument, NULL, 0},
    {"tcp-base-port", required_argument, NULL, 0},
    {"tcp-server-ip", required_argument, NULL, 0},
    {"msg-size", required_argument, NULL, 0},
    {NULL, 0, NULL, 0}
};

/** Set from a signal handler to request that the client threads in this process send messages to request the test stops */
static volatile bool stop_transmission;

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
    printf ("  ibv_round_trip_delay <options>  Test round-trip delay for TCP .vs. RDMA\n");
    printf ("\n");
    printf ("Options:\n");
    printf ("  --role=rdma_server|rdma_client|tcp_server|tcp_client\n");
    printf ("    Specifies the role performed by the program, in terms of the transport\n");
    printf ("    and which endpoint.\n");
    printf ("  --cores=<cores>  Specifies the comma separated list of cores to run message\n");
    printf ("                   passing threads on.\n");
    printf ("  --ib-dev=<dev>  Specifies the Infiniband device used for RDMA transport\n");
    printf ("  --ib-port=<port>  Specifies the Infiniband device used for RDMA transport\n");
    printf ("  --tcp-base-port=<port>  Specifies the base TCP port number used for TCP\n");
    printf ("                          transport, incremented for each thread.\n");
    printf ("  --tcp-server-ip=<addr>  Specifies the IP address of the TCP server,\n");
    printf ("                          to which the tcp_client connects.\n");
    printf ("  --msg-size=<size>  The data size in bytes of the message exchanged.\n");

    exit (EXIT_FAILURE);
}


/**
 * @brief Process the cores command line argument which is a comma-separated list
 * @param[in] optdef Option definition
 */
static void process_cores_argument (const struct option *const optdef)
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
            (core < 0) || (core >= num_cpus))
        {
            fprintf (stderr, "Invalid %s %s\n", optdef->name, token);
            exit (EXIT_FAILURE);
        }
        if (arg_num_cores < MAX_THREADS)
        {
            arg_cores[arg_num_cores] = core;
            arg_num_cores++;
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
 * @brief Parse command line arguments, storing the result in global variables
 * @details Aborts the program if invalid arguments
 */
static void parse_command_line_arguments (const int argc, char *argv[])
{
    int opt_status;
    char junk;
    int rc;
    bool test_role_set = false;
    bool ib_port_set = false;
    bool tcp_server_addr_set = false;

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
            else if (strcmp (optdef->name, "role") == 0)
            {
                if (strcmp (optarg, "rdma_server") == 0)
                {
                    arg_test_role = TEST_ROLE_RDMA_SERVER;
                }
                else if (strcmp (optarg, "rdma_client") == 0)
                {
                    arg_test_role = TEST_ROLE_RDMA_CLIENT;
                }
                else if (strcmp (optarg, "tcp_server") == 0)
                {
                    arg_test_role = TEST_ROLE_TCP_SERVER;
                }
                else if (strcmp (optarg, "tcp_client") == 0)
                {
                    arg_test_role = TEST_ROLE_TCP_CLIENT;
                }
                else
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
                test_role_set = true;
            }
            else if (strcmp (optdef->name, "cores") == 0)
            {
                process_cores_argument (optdef);
            }
            else if (strcmp (optdef->name, "ib-dev") == 0)
            {
                arg_ib_device = strdup (optarg);
            }
            else if (strcmp (optdef->name, "ib-port") == 0)
            {
                if ((sscanf (optarg, "%u%c", &arg_ib_port, &junk) != 1) || (arg_ib_port > 255))
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
                ib_port_set = true;
            }
            else if (strcmp (optdef->name, "tcp-base-port") == 0)
            {
                if ((sscanf (optarg, "%u%c", &arg_tcp_base_port, &junk) != 1) || (arg_tcp_base_port > 65535))
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else if (strcmp (optdef->name, "tcp-server-ip") == 0)
            {
                rc = inet_pton (AF_INET, optarg, &arg_tcp_server_addr);
                if (rc != 1)
                {
                    fprintf (stderr, "Invalid %s %s\n", optdef->name, optarg);
                    exit (EXIT_FAILURE);
                }
                tcp_server_addr_set = true;
            }
            else if (strcmp (optdef->name, "msg-size") == 0)
            {
                if ((sscanf (optarg, "%u%c", &arg_message_size, &junk) != 1) || (arg_message_size < 4))
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

    if (arg_num_cores == 0)
    {
        fprintf (stderr, "At least one core must be specified\n");
        exit (EXIT_FAILURE);
    }

    if (!test_role_set)
    {
        fprintf (stderr, "The test role must be specified\n");
        exit (EXIT_FAILURE);
    }

    switch (arg_test_role)
    {
    case TEST_ROLE_RDMA_SERVER:
    case TEST_ROLE_RDMA_CLIENT:
        if ((arg_ib_device == NULL) || (!ib_port_set))
        {
            fprintf (stderr, "Both ib-dev and ib-port must be specified when using RDMA transport\n");
            exit (EXIT_FAILURE);
        }
        break;

    case TEST_ROLE_TCP_CLIENT:
        if (!tcp_server_addr_set)
        {
            fprintf (stderr, "tcp-server-ip must be specified for tcp_client\n");
            exit (EXIT_FAILURE);
        }
        break;

    case TEST_ROLE_TCP_SERVER:
        /* No required options */
        break;
    }
}

/**
 * @brief Initialise the RDMA transport for one message thread.
 * @details Returns once has connected to the remote thread.
 * @param[in,out] context The message thread context being initialised.
 */
static void round_trip_message_thread_rdma_initialise (message_thread_context *const context)
{
    const int server_node = (context->thread_index * NUM_NODES_PER_THREAD) + SERVER_NODE_OFFSET;
    const int client_node = (context->thread_index * NUM_NODES_PER_THREAD) + CLIENT_NODE_OFFSET;

    if (arg_test_role == TEST_ROLE_RDMA_SERVER)
    {
        context->rdma.local_node_number = server_node;
        context->rdma.remote_node_number = client_node;
    }
    else
    {
        context->rdma.local_node_number = client_node;
        context->rdma.remote_node_number = server_node;
    }
    context->rdma.node_context = communication_context_initialise (context->rdma.local_node_number);
    context->rdma.tx_handle = get_tx_path_handle (context->rdma.node_context, context->rdma.remote_node_number, 0);
    printf ("RDMA %s %d connected\n", (arg_test_role == TEST_ROLE_RDMA_SERVER) ? "server" : "client", context->thread_index);
}


/**
 * @brief Initialise the TCP transport for one message thread.
 * @details Returns once has connected to the remote thread.
 * @param[in,out] context The message thread context being initialised.
 */
static void round_trip_message_thread_tcp_initialise (message_thread_context *const context)
{
    int rc;

    if (arg_test_role == TEST_ROLE_TCP_SERVER)
    {
        /* Create a listening server socket, and wait for the client to connect.
         * The listening socket can be closed after the client has connected since there is a separate server port for each
         * part of client and server message threads. */
        int listening_socket;

        listening_socket = socket (PF_INET, SOCK_STREAM, 0);
        check_assert (listening_socket >= 0, "socket");

        int reuse_addr = 1;
        rc = setsockopt (listening_socket, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, sizeof (reuse_addr));
        check_assert (rc == 0, "setsockopt");

        struct sockaddr_in server_addr =
        {
            .sin_family = AF_INET,
            .sin_port = htons (arg_tcp_base_port + context->thread_index)
        };
        rc = bind (listening_socket, &server_addr, sizeof (server_addr));
        check_assert (rc == 0, "bind");

        const int max_backlog = 1;
        rc = listen (listening_socket, max_backlog);
        check_assert (rc == 0, "listen");

        struct sockaddr_in client_addr;
        socklen_t client_addr_len = sizeof (client_addr);
        context->tcp.data_socket = accept (listening_socket, &client_addr, &client_addr_len);
        check_assert (context->tcp.data_socket >= 0, "accept");

        rc = close (listening_socket);
        check_assert (rc == 0, "close");

        char client_addr_str[INET_ADDRSTRLEN];
        inet_ntop (AF_INET, &client_addr.sin_addr, client_addr_str, sizeof (client_addr_str));
        printf ("TCP server %d accepted connection from client %s:%u\n",
                context->thread_index, client_addr_str, ntohs (client_addr.sin_port));
    }
    else
    {
        /* Connect to the server */
        context->tcp.data_socket = socket (PF_INET, SOCK_STREAM, 0);
        check_assert (context->tcp.data_socket >= 0, "socket");

        struct sockaddr_in server_addr =
        {
            .sin_family = AF_INET,
            .sin_port = htons (arg_tcp_base_port + context->thread_index),
            .sin_addr = arg_tcp_server_addr
        };

        rc = connect (context->tcp.data_socket, &server_addr, sizeof (server_addr));
        check_assert (rc == 0, strerror (errno));
        printf ("TCP client %d connected\n", context->thread_index);
    }

    /* Disable Nagle, as are exchanging short messages as quickly as possible */
    int nodelay = 1;
    rc = setsockopt (context->tcp.data_socket, IPPROTO_TCP, TCP_NODELAY, &nodelay, sizeof (nodelay));
    check_assert (rc == 0, "setsockopt");

    /* Set non-blocking socket as should only be one short outstanding message */
    rc = fcntl (context->tcp.data_socket, F_SETFL, O_NONBLOCK);
    check_assert (rc == 0, "fnctl");

    /* Allocate message buffers */
    context->tcp.buffer_size_bytes = sizeof (uint32_t) /* header for message ID */ + arg_message_size;
    context->tcp.tx_buffer = malloc (context->tcp.buffer_size_bytes);
    CHECK_ASSERT (context->tcp.tx_buffer != NULL);
    context->tcp.rx_buffer = malloc (context->tcp.buffer_size_bytes);
    CHECK_ASSERT (context->tcp.rx_buffer != NULL);
}


/**
 * @brief Wait for a TCP message to be received.
 * @details This is done by using ioctl() to poll for the number of bytes to be that of the fixed size message used by the test,
 *          and then performing a non-blocking read() when there are sufficient available bytes for one message.
 *          Performing a busy-poll read is done for comparing against the RDMA transport which is also busy-polling.
 * @param[in/out] context The TCP transport context to read the message from.
 */
static void await_tcp_message (message_thread_tcp_context *const context)
{
    int available_bytes;
    int rc;

    do
    {
        rc = ioctl (context->data_socket, FIONREAD, &available_bytes);
        check_assert (rc == 0, "ioctl FIONREAD");
    } while (available_bytes < context->buffer_size_bytes);

    const ssize_t bytes_read = read (context->data_socket, context->rx_buffer, context->buffer_size_bytes);
    CHECK_ASSERT (bytes_read == (ssize_t) context->buffer_size_bytes);
}


/**
 * @brief Send a TCP message
 * @details Since as sending short messages, with only one outstanding at a time, assume that the non-blocking send
 *          will always transmit the entire message.
 * @param[in] context The TCP transport message to transmit the message from.
 */
static void send_tcp_message (const message_thread_tcp_context *const context)
{
    const ssize_t bytes_written = write (context->data_socket, context->tx_buffer, context->buffer_size_bytes);
    CHECK_ASSERT (bytes_written == (ssize_t) context->buffer_size_bytes);
}


/**
 * @details Perform one iteration of the round trip message test for a thread, which involves a message exchange with the
 *          remote node.
 *          On the client this poll the flag to stop the test, and the remove server will exit when send the message ID
 *          which indicates the test is complete.
 * @param[in,out] The message thread context.
 */
static void round_trip_message_iteration (message_thread_context *const context)
{
    rx_api_message_buffer *rx_buffer;
    tx_api_message_buffer *tx_buffer;

    switch (arg_test_role)
    {
    case TEST_ROLE_RDMA_SERVER:
        /* Echo received messages back to the client */
        rx_buffer = await_any_rx_message (context->rdma.node_context);
        context->test_complete = rx_buffer->header->message_id == EXIT_MSG_ID;

        tx_buffer = get_send_buffer (context->rdma.tx_handle);
        tx_buffer->header->message_id = rx_buffer->header->message_id;
        tx_buffer->header->message_length = rx_buffer->header->message_length;
        memcpy (tx_buffer->data, rx_buffer->data, tx_buffer->header->message_id);
        send_message (tx_buffer);

        free_message (rx_buffer);
        break;

    case TEST_ROLE_RDMA_CLIENT:
        /* Send a message to server and wait for it to be echoed back */
        tx_buffer = get_send_buffer (context->rdma.tx_handle);
        tx_buffer->header->message_id = stop_transmission ? EXIT_MSG_ID : ECHO_MSG_ID;
        tx_buffer->header->message_length = arg_message_size;
        send_message (tx_buffer);

        rx_buffer = await_any_rx_message (context->rdma.node_context);
        context->test_complete = rx_buffer->header->message_id == EXIT_MSG_ID;
        free_message (rx_buffer);
        break;

    case TEST_ROLE_TCP_SERVER:
        /* Echo received messages back to the client */
        await_tcp_message (&context->tcp);
        context->test_complete = context->tcp.rx_buffer[0] == EXIT_MSG_ID;

        memcpy (context->tcp.tx_buffer, context->tcp.rx_buffer, context->tcp.buffer_size_bytes);
        send_tcp_message (&context->tcp);
        break;

    case TEST_ROLE_TCP_CLIENT:
        /* Send a message to server and wait for it to be echoed back */
        context->tcp.tx_buffer[0] = stop_transmission ? EXIT_MSG_ID : ECHO_MSG_ID;
        send_tcp_message (&context->tcp);

        await_tcp_message (&context->tcp);
        context->test_complete = context->tcp.rx_buffer[0] == EXIT_MSG_ID;
        break;
    }

    context->results.total_messages++;
}


/**
 * @brief Thread entry point for one message thread to perform round-trip measurement.
 * @details The thread is either once instance of a client or server endpoint, communicating with the remote instance
 *          using either RDMA or TCP transport.
 * @param[in,out] arg The context for the thread
 * @return Not used
 */
static void *round_trip_message_thread (void *const arg)
{
    message_thread_context *const context = arg;
    int rc;

    /* Set the memory policy for the thread to allocate from the local NUMA node (according to the core affinity) */
    rc = set_mempolicy (MPOL_BIND | MPOL_F_STATIC_NODES, context->numa_mask->maskp, context->numa_mask->size);
    check_assert (rc == 0, "set_mempolicy");

    /* Initialise the message transfer */
    switch (arg_test_role)
    {
    case TEST_ROLE_RDMA_SERVER:
    case TEST_ROLE_RDMA_CLIENT:
        round_trip_message_thread_rdma_initialise (context);
        break;

    case TEST_ROLE_TCP_SERVER:
    case TEST_ROLE_TCP_CLIENT:
        round_trip_message_thread_tcp_initialise (context);
        break;
    }

    /* Perform an initial exchange to ensure both ends are ready */
    context->test_complete = false;
    round_trip_message_iteration (context);

    /* The message initial exchanged is not counted towards the final results, as may have to wait for the remote end to
     * be ready. */
    context->results.total_messages = 0u;

    /* Start measurements when all message threads ready */
    rc = pthread_barrier_wait (&threads_ready_barrier);
    check_assert ((rc == 0) || (rc == PTHREAD_BARRIER_SERIAL_THREAD), "pthread_barrier_wait");

    rc = getrusage (RUSAGE_THREAD, &context->results.start_usage);
    check_assert (rc == 0, "getrusage");
    clock_gettime (CLOCK_MONOTONIC, &context->results.start_time);
    while (!context->test_complete)
    {
        round_trip_message_iteration (context);
    }
    clock_gettime (CLOCK_MONOTONIC, &context->results.stop_time);
    rc = getrusage (RUSAGE_THREAD, &context->results.stop_usage);
    check_assert (rc == 0, "getrusage");

    /* Finalise the message transfer */
    switch (arg_test_role)
    {
    case TEST_ROLE_RDMA_SERVER:
    case TEST_ROLE_RDMA_CLIENT:
        communication_context_finalise (context->rdma.node_context);
        break;

    case TEST_ROLE_TCP_SERVER:
    case TEST_ROLE_TCP_CLIENT:
        rc = close (context->tcp.data_socket);
        check_assert (rc == 0, "close");
        break;
    }

    return NULL;
}


/**
 * @brief Register a bi-directional path between each instance of the client and server nodes
 */
static void register_round_trip_communication_paths (void)
{
    uint32_t thread_index;

    for (thread_index = 0; thread_index < arg_num_cores; thread_index++)
    {
        const int server_node = (thread_index * NUM_NODES_PER_THREAD) + SERVER_NODE_OFFSET;
        const int client_node = (thread_index * NUM_NODES_PER_THREAD) + CLIENT_NODE_OFFSET;
        communication_path_definition path_def =
        {
            .instance = 0,
            .source_ib_device = arg_ib_device,
            .destination_ib_device = arg_ib_device,
            .source_port_num = arg_ib_port,
            .destination_port_num = arg_ib_port,
            .service_level = DEFAULT_SERVICE_LEVEL,
            .num_message_buffers = 64, /* While only one message can be outstanding, a larger value reduces CQ handling */
            .max_message_size = arg_message_size,
            .allocation_type = BUFFER_ALLOCATION_SHARED_MEMORY,
            .tx_checks_memory_buffer_size = true,
            .tx_polls_for_errors = false,
            .set_non_default_retry_timeout = false
        };

        path_def.source_node = client_node;
        path_def.destination_node = server_node;
        register_path_definition (&path_def);
        path_def.source_node = server_node;
        path_def.destination_node = client_node;
        register_path_definition (&path_def);
    }
}


/**
 * @brief Create the message threads specified by the command line options.
 */
static void create_round_trip_message_threads (void)
{
    int rc;
    pthread_attr_t thread_attr;
    cpu_set_t cpuset;
    uint32_t thread_index;
    int numa_node;

    rc = pthread_barrier_init (&threads_ready_barrier, NULL, 1 /* main thread */ + arg_num_cores);
    check_assert (rc == 0, "pthread_barrier_init");

    for (thread_index = 0; thread_index < arg_num_cores; thread_index++)
    {
        message_thread_context *const context = &thread_contexts[thread_index];

        context->thread_index = thread_index;
        context->thread_joined = false;

        /* Set CPU affinity for the message thread specified by the command line options.
         * Also determine the local NUMA node mask to be used for allocations. */
        rc = pthread_attr_init (&thread_attr);
        check_assert (rc == 0, "pthread_attr_init");
        CPU_ZERO (&cpuset);
        CPU_SET (arg_cores[thread_index], &cpuset);
        rc = pthread_attr_setaffinity_np (&thread_attr, sizeof (cpuset), &cpuset);
        check_assert (rc == 0, "pthread_attr_setaffinty_np");

        context->numa_mask = numa_allocate_nodemask ();
        check_assert (context->numa_mask != NULL, "numa_allocate_nodemask");
        numa_node = numa_node_of_cpu (arg_cores[thread_index]);
        check_assert (numa_node != -1, "numa_node_of_cpu");
        numa_bitmask_setbit (context->numa_mask, (unsigned int) numa_node);

        rc = pthread_create (&context->thread_id, &thread_attr, round_trip_message_thread, context);
        check_assert (rc == 0, "pthread_create");
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
 * @brief Report the progress for one message thread during a test
 * @param[in] thread_index Which thread to report the progress for
 */
static void report_message_thread_regular_progress (const uint32_t thread_index)
{
    message_thread_context *const context = &thread_contexts[thread_index];

    /* Sample the current results */
    context->previous_results = context->current_results;
    context->previous_results_time = context->current_results_time;
    context->current_results = context->results;
    clock_gettime (CLOCK_MONOTONIC, &context->current_results_time);

    /* Report the results */
    if (context->current_results.total_messages > 0)
    {
        const double report_interval_secs =
                get_elapsed_ns (&context->previous_results_time, &context->current_results_time) / 1E9;

        printf ("thread %u %lu messages over last %.1f seconds\n",
                thread_index,
                context->current_results.total_messages - context->previous_results.total_messages,
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

    /* Start measurements when all message threads ready */
    rc = pthread_barrier_wait (&threads_ready_barrier);
    check_assert ((rc == 0) || (rc == PTHREAD_BARRIER_SERIAL_THREAD), "pthread_barrier_wait");

    num_threads_running = arg_num_cores;
    clock_gettime (CLOCK_REALTIME, &abs_time);

    for (thread_index = 0; thread_index < arg_num_cores; thread_index++)
    {
        clock_gettime (CLOCK_MONOTONIC, &thread_contexts[thread_index].current_results_time);
    }

    if ((arg_test_role == TEST_ROLE_RDMA_CLIENT) || (arg_test_role == TEST_ROLE_TCP_CLIENT))
    {
        /* Install a signal handler to allow a request to stop transmission */
        struct sigaction action;

        printf ("Press Ctrl-C to tell the %u client thread(s) to stop the test\n", arg_num_cores);
        memset (&action, 0, sizeof (action));
        action.sa_handler = stop_transmission_handler;
        action.sa_flags = SA_RESTART;
        rc = sigaction (SIGINT, &action, NULL);
        check_assert (rc == 0, "sigaction");
    }

    /* While any thread is still running try and join a running thread.
     * Where the timeout is used to trigger the reporting of progress. */
    while (num_threads_running > 0)
    {
        thread_index = 0;
        while ((thread_index < arg_num_cores) && (thread_contexts[thread_index].thread_joined))
        {
            thread_index++;
        }
        abs_time.tv_sec += report_interval_seconds;
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
            for (thread_index = 0; thread_index < arg_num_cores; thread_index++)
            {
                report_message_thread_regular_progress (thread_index);
            }
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
 * @details Display the final results for one message thread which is:
 *          - The total number of messages exchanged
 *          - The Linux usage measurements
 * @param[in] thread_index Which thread to display the results for
 */
static void display_message_thread_final_results (const uint32_t thread_index)
{
    message_round_trip_results *const results = &thread_contexts[thread_index].results;

    printf ("\n");
    /* Display textual results for the number messages for the entire run */
    const double test_duration_secs = (double) get_elapsed_ns (&results->start_time, &results->stop_time) / 1E9;

    printf ("%u Total messages %lu over %.6f seconds; %.0f messages/second\n",
            thread_index,
            results->total_messages, test_duration_secs,
            (double) results->total_messages / test_duration_secs);

    /* Display resource usage for the entire run */
    printf ("%u minor page faults=%ld (%ld -> %ld)\n",
            thread_index,
            results->stop_usage.ru_minflt - results->start_usage.ru_minflt,
            results->start_usage.ru_minflt, results->stop_usage.ru_minflt);
    printf ("%u major page faults=%ld (%ld -> %ld)\n",
            thread_index,
            results->stop_usage.ru_majflt - results->start_usage.ru_majflt,
            results->start_usage.ru_majflt, results->stop_usage.ru_majflt);
    printf ("%u voluntary context switches=%ld (%ld -> %ld)\n",
            thread_index,
            results->stop_usage.ru_nvcsw - results->start_usage.ru_nvcsw,
            results->start_usage.ru_nvcsw, results->stop_usage.ru_nvcsw);
    printf ("%u involuntary context switches=%ld (%ld -> %ld)\n",
            thread_index,
            results->stop_usage.ru_nivcsw - results->start_usage.ru_nivcsw,
            results->start_usage.ru_nivcsw, results->stop_usage.ru_nivcsw);
    printf ("%u user time=%.6f system time=%.6f\n",
            thread_index,
            rusage_utilisation_secs (&results->start_usage.ru_utime, &results->stop_usage.ru_utime),
            rusage_utilisation_secs (&results->start_usage.ru_stime, &results->stop_usage.ru_stime));
}


int main (int argc, char *argv[])
{
    int rc;
    SLPError slp_status;
    SLPHandle slp_main_thread_handle;
    uint32_t thread_index;

    parse_command_line_arguments (argc, argv);

    rc = numa_available ();
    check_assert (rc != -1, "numa_available");

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    check_assert (rc == 0, "mlockall");

    if ((arg_test_role == TEST_ROLE_RDMA_SERVER) || (arg_test_role == TEST_ROLE_RDMA_CLIENT))
    {
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

        register_round_trip_communication_paths ();
    }
    create_round_trip_message_threads ();
    wait_for_message_threads_to_exit ();
    for (thread_index = 0; thread_index < arg_num_cores; thread_index++)
    {
        display_message_thread_final_results (thread_index);
    }

    return EXIT_SUCCESS;
}
