/*
 * @file udp_broadcast.c
 * @date 7 Aug 2022
 * @author Chester Gillon
 */

#define _GNU_SOURCE /* Avoids the need for a cast in the address parameter to bind() */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <sys/socket.h>
#include <arpa/inet.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <poll.h>
#include <net/if.h>
#include <ifaddrs.h>

#define NSECS_PER_SEC 1000000000LL

/* The transmit rate used by this program */
#define DATAGRAM_TRANSMIT_RATE_HZ 100

/* Fixed UDP source and destination port number used by program */
#define UDP_PORT_NUM 5000

#define MAX_SOURCE_IPS 10

/* Defines the UDP test messages sent by this program */
typedef struct
{
    /* Sequence number incremented for every message sent */
    uint64_t sequence_number;
    /* A message payload which is just the dotted decimal source IP address inserted by the sending program.
     * Should match the source address obtained from recvfrom(). */
    char source_ip_addr_string[INET6_ADDRSTRLEN];
} test_msg_t;

/* Used to record the messages received from one source address */
typedef struct
{
    /* The IPv4 source address the message was received from */
    in_addr_t s_addr;
    /* The total number of messages received from the source */
    uint32_t num_messages_received;
    /* The first and last messages received from the source. The difference between the sequence numbers can be compared
     * against num_messages_received to determine how many messages were not seen. */
    test_msg_t first_message;
    test_msg_t last_message;
} messages_from_source_t;

/* Contains the test context which is per source IP address */
typedef struct
{
    /* The IP address string saved for use in reporting results */
    char ip_addr_string[INET_ADDRSTRLEN];
    /* The source address for packets transmitted by this program */
    struct in_addr source_addr;
    /* The name of the interface which the source_addr is on */
    char ifa_name[IF_NAMESIZE];
    /* The socket used to transmit, which is bound to source_addr */
    int tx_socket;
    /* The socket used to receive, which is bound to the interface for source_addr */
    int rx_socket;
    /* Used to transmit messages from the socket */
    test_msg_t tx_message;
    /* The number of messages sent with a successful sendto() return */
    uint32_t num_messages_sent;
    /* The number of tried to sent a message, but sendto() returned a failure */
    uint32_t num_failed_sends;
    /* Used to receive messages from the socket */
    test_msg_t rx_message;
    /* The number of different source addresses from which messages were received.
     * The number of valid entries in the messages_per_source[] array. */
    uint32_t num_received_sources;
    messages_from_source_t messages_per_source[MAX_SOURCE_IPS];
} per_source_ip_t;


/* The source IP tested by this program, the number set from command line arguments */
static uint32_t num_source_ips;
static per_source_ip_t source_ips[MAX_SOURCE_IPS];


/* Set from a signal handler to request the test is stopped */
static volatile bool stop_transmission;

/**
 * @brief Signal handler to request transmission of messages is stopped
 */
static void stop_transmission_handler (const int sig)
{
    stop_transmission = true;
}


/*
 * @brief Return a monotonic time in integer nanoseconds
 */
static int64_t get_monotonic_time (void)
{
    int rc;
    struct timespec now;

    rc = clock_gettime (CLOCK_MONOTONIC, &now);
    if (rc != 0)
    {
        printf ("clock_getime(CLOCK_MONOTONIC) failed\n");
        exit (EXIT_FAILURE);
    }

    return (now.tv_sec * NSECS_PER_SEC) + now.tv_nsec;
}


int main (int argc, char *argv[])
{
    int rc;
    int saved_errno;
    uint32_t ip_index;
    struct pollfd poll_fds[MAX_SOURCE_IPS] = {0};

    if (argc < 2)
    {
        printf ("Usage: %s <source_IPv4_1> [ ... <source_IPv4_N> ]\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    /* Get all interfaces to be able to lookup interface for source IP address */
    struct ifaddrs *interface_addresses = NULL;
    rc = getifaddrs (&interface_addresses);
    if (rc != 0)
    {
        printf ("getifaddrs() failed\n");
        return EXIT_FAILURE;
    }

    /* Parse command line arguments, converting the IPv4 dotted decimal source addresses */
    for (int arg_index = 1; arg_index < argc; arg_index++)
    {
        per_source_ip_t *const source_ip = &source_ips[num_source_ips];
        const char *const ip_addr_string = argv[arg_index];

        if (num_source_ips == MAX_SOURCE_IPS)
        {
            printf ("Number of source IPs exceeds compiled in maximum\n");
            exit (EXIT_FAILURE);
        }

        /* Convert source address string into internal format */
        snprintf (source_ip->ip_addr_string, sizeof (source_ip->ip_addr_string), "%s", ip_addr_string);
        rc = inet_pton (AF_INET, ip_addr_string, &source_ip->source_addr);
        if (rc != 1)
        {
            printf ("%s is not a valid IPv4 address\n", ip_addr_string);
            exit (EXIT_FAILURE);
        }

        /* Locate the interface which the source address is on */
        struct ifaddrs *iap = interface_addresses;
        bool found_interface = false;
        while ((!found_interface) && (iap != NULL))
        {
            if ((iap->ifa_addr != NULL) && ((iap->ifa_flags & IFF_UP) == IFF_UP) && (iap->ifa_addr->sa_family == AF_INET))
            {
                struct sockaddr_in *const sa = (struct sockaddr_in *) iap->ifa_addr;

                if (sa->sin_addr.s_addr == source_ip->source_addr.s_addr)
                {
                    snprintf (source_ip->ifa_name, sizeof (source_ip->ifa_name), "%s", iap->ifa_name);
                    found_interface = true;
                }
            }
            iap = iap->ifa_next;
        }

        if (found_interface)
        {
            printf ("source address %s is on interface %s\n", source_ip->ip_addr_string, source_ip->ifa_name);
        }
        else
        {
            printf ("Unable to find source address %s on any network interface which is UP\n", source_ip->ip_addr_string);
            exit (EXIT_FAILURE);
        }

        num_source_ips++;
    }

    /* Initialise the sockets used for each source IP address */
    for (ip_index = 0; ip_index < num_source_ips; ip_index++)
    {
        per_source_ip_t *const source_ip = &source_ips[ip_index];

        /* Create a non-blocking UDP transmit socket */
        source_ip->tx_socket = socket (AF_INET, SOCK_DGRAM | SOCK_NONBLOCK, 0);
        if (source_ip->tx_socket == -1)
        {
            printf ("socket() failed\n");
            exit (EXIT_FAILURE);
        }

        int reuse_port = 1;
        rc = setsockopt (source_ip->tx_socket, SOL_SOCKET, SO_REUSEPORT, &reuse_port, sizeof (reuse_port));
        if (rc != 0)
        {
            printf ("Failed to reuse port for transmit socket\n");
            exit (EXIT_FAILURE);
        }

        /* Bind to the specified source IP address and fixed port number for transmit */
        struct sockaddr_in tx_addr =
        {
            .sin_family = AF_INET,
            .sin_port = htons (UDP_PORT_NUM),
            .sin_addr = source_ip->source_addr
        };
        errno = 0;
        rc = bind (source_ip->tx_socket, &tx_addr, sizeof (tx_addr));
        saved_errno = errno;
        if (rc != 0)
        {
            printf ("Failed to bind tx_socket to %s : %s\n", source_ip->ip_addr_string, strerror (saved_errno));
            exit (EXIT_FAILURE);
        }

        /* Allow broadcast on the transmit socket */
        int enable_broadcast = 1;
        rc = setsockopt (source_ip->tx_socket, SOL_SOCKET, SO_BROADCAST, &enable_broadcast, sizeof (enable_broadcast));
        if (rc != 0)
        {
            printf ("setsockopt (SO_BROADCAST) failed\n");
            exit (EXIT_FAILURE);
        }

        /* Create a non-blocking UDP receive socket */
        source_ip->rx_socket = socket (AF_INET, SOCK_DGRAM | SOCK_NONBLOCK, 0);
        if (source_ip->rx_socket == -1)
        {
            printf ("socket() failed\n");
            exit (EXIT_FAILURE);
        }
        poll_fds[ip_index].fd = source_ip->rx_socket;
        poll_fds[ip_index].events = POLLIN;

        /* Need to bind the receive socket to the device to receive broadcasts only delivered to the interface.
         * The transmit socket is bound to the source IP address.
         * SO_BINDTODEVICE requires CAP_NET_RAW.
         *
         * @todo This seems to receive a copy of the transmitted packets.
         *       Possible to make use of IP_PKTINFO as per https://stackoverflow.com/questions/3062205/setting-the-source-ip-for-a-udp-socket ?
         * */
        errno = 0;
        rc = setsockopt (source_ip->rx_socket, SOL_SOCKET, SO_BINDTODEVICE, source_ip->ifa_name, sizeof (source_ip->ifa_name));
        saved_errno = errno;
        if (rc != 0)
        {
            printf ("Failed to bind to device %s : %s\n", source_ip->ifa_name, strerror (saved_errno));
            exit (EXIT_FAILURE);
        }

        rc = setsockopt (source_ip->rx_socket, SOL_SOCKET, SO_REUSEPORT, &reuse_port, sizeof (reuse_port));
        if (rc != 0)
        {
            printf ("Failed to reuse port for receive socket\n");
            exit (EXIT_FAILURE);
        }

        /* Bind to the specified fixed port number for receive */
        struct sockaddr_in rx_addr =
        {
            .sin_family = AF_INET,
            .sin_port = htons (UDP_PORT_NUM),
        };
        errno = 0;
        rc = bind (source_ip->rx_socket, &rx_addr, sizeof (rx_addr));
        saved_errno = errno;
        if (rc != 0)
        {
            printf ("Failed to bind rx_socket to %s : %s\n", source_ip->ip_addr_string, strerror (saved_errno));
            exit (EXIT_FAILURE);
        }

        source_ip->num_messages_sent = 0;
        source_ip->num_failed_sends = 0;
        source_ip->tx_message.sequence_number = 0;
        snprintf (source_ip->tx_message.source_ip_addr_string, sizeof (source_ip->tx_message.source_ip_addr_string), "%s",
                source_ip->ip_addr_string);
        source_ip->num_received_sources = 0;
    }

    const struct sockaddr_in broadcast_addr =
    {
        .sin_family = AF_INET,
        .sin_port = htons (UDP_PORT_NUM),
        .sin_addr.s_addr = INADDR_BROADCAST
    };

    /* Run test until requested to stop */
    struct sigaction action;

    printf ("Press Ctrl-C to stop the test\n");
    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_transmission_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    if (rc != 0)
    {
        printf ("sigaction() failed\n");
        exit (EXIT_FAILURE);
    }

    const int64_t send_interval_ns = NSECS_PER_SEC / DATAGRAM_TRANSMIT_RATE_HZ;
    const int64_t test_start_time = get_monotonic_time ();
    int64_t time_of_next_send = test_start_time;

    while (!stop_transmission)
    {
        const int64_t now = get_monotonic_time ();

        /* Send a message from each source IP address, when get to the time for the next send */
        if (now >= time_of_next_send)
        {
            for (ip_index = 0; ip_index < num_source_ips; ip_index++)
            {
                per_source_ip_t *const source_ip = &source_ips[ip_index];

                rc = sendto (source_ip->tx_socket, &source_ip->tx_message, sizeof (source_ip->tx_message), 0,
                        &broadcast_addr, sizeof (broadcast_addr));
                if (rc == sizeof (source_ip->tx_message))
                {
                    source_ip->num_messages_sent++;
                }
                else
                {
                    source_ip->num_failed_sends++;
                }
                source_ip->tx_message.sequence_number++;
            }
            time_of_next_send += send_interval_ns;
        }

        /* Poll for received messages on any socket */
        rc = poll (poll_fds, num_source_ips, 0);
        for (ip_index = 0; ip_index < num_source_ips; ip_index++)
        {
            per_source_ip_t *const source_ip = &source_ips[ip_index];
            struct sockaddr_in remote_addr;
            socklen_t remote_addr_len = sizeof (remote_addr);

            if ((poll_fds[ip_index].revents & POLLIN) == POLLIN)
            {
                rc = recvfrom (source_ip->rx_socket, &source_ip->rx_message, sizeof (source_ip->rx_message), 0,
                        &remote_addr, &remote_addr_len);
                if ((rc == sizeof (source_ip->rx_message)) && (remote_addr_len == sizeof (remote_addr)))
                {
                    bool found_existing_source = false;

                    for (uint32_t rx_source_index = 0;
                            (!found_existing_source) && (rx_source_index < source_ip->num_received_sources);
                            rx_source_index++)
                    {
                        messages_from_source_t *const existing_source = &source_ip->messages_per_source[rx_source_index];

                        if (remote_addr.sin_addr.s_addr == existing_source->s_addr)
                        {
                            existing_source->num_messages_received++;
                            existing_source->last_message = source_ip->rx_message;
                            found_existing_source = true;
                        }
                    }

                    if ((!found_existing_source) && (source_ip->num_received_sources < MAX_SOURCE_IPS))
                    {
                        messages_from_source_t *const new_source = &source_ip->messages_per_source[source_ip->num_received_sources];

                        new_source->s_addr = remote_addr.sin_addr.s_addr;
                        new_source->num_messages_received = 1;
                        new_source->first_message = source_ip->rx_message;
                        new_source->last_message = source_ip->rx_message;
                        source_ip->num_received_sources++;
                    }
                }
            }
        }
    }

    const int64_t test_end_time = get_monotonic_time ();
    const double test_duration_seconds = (double) (test_end_time - test_start_time) / 1E9;

    printf ("Test ran for %.3f seconds\n", test_duration_seconds);

    return EXIT_SUCCESS;
}
