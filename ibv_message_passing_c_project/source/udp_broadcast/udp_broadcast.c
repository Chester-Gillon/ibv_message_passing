/*
 * @file udp_broadcast.c
 * @date 7 Aug 2022
 * @author Chester Gillon
 * @brief Program to investigate UDP IPv4 broadcast under Linux, for both transmit and receive.
 * @details
 *   Command line arguments specify one or more source IPv4 addresses, and for each address:
 *   1. One socket is created to transmit/receive.
 *      The socket is bound to only the UDP port number, and no specific IP address.
 *      This is so that can receive broadcasts.
 *      The sendmsg() IP_PKTINFO ancillary data is used to set the source IP address for the broadcasts.
 *   2. Sends UDP broadcasts at a fixed rate, using the specified source IPv4 address.
 *   3. Poll for receipt. The same UDP source and destination port number is used for all sockets, so can tell
 *      if receive "copies" of broadcast packets.
 *      The source IP address and interface on the the packet was received from is reported, which identifies
 *      if broadcasts are being received by multiple interfaces.
 *
 *   A command line option can enable the use of SO_BINDTODEVICE to cause the receive sockets to only
 *   receive packets from the specific interface on which the source IP address is set.
 *   Use of SO_BINDTODEVICE can be used to investigate filtering of "copies" of broadcasts received on multiple
 *   interfaces, which may have some interaction with the net.ipv4.conf.all.rp_filter setting in the network stack.
 *
 *   As well as reporting the test packets received from a UDP socket, also reads the ethtool broadcast statistics
 *   on all interfaces on the host so that can tell if copies of broadcasts are being received on all network
 *   interfaces or not. This is to support testing of VLANs to create separate "domains" to isolate broadcasts.
 */

#define _GNU_SOURCE /* Avoids the need for a cast in the address parameter to bind() */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include <getopt.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <poll.h>
#include <net/if.h>
#include <ifaddrs.h>
#include <sys/ioctl.h>
#include <linux/ethtool.h>
#include <linux/sockios.h>

#define NSECS_PER_SEC 1000000000LL

/* The transmit rate used by this program */
#define DATAGRAM_TRANSMIT_RATE_HZ 100

/* Fixed UDP source and destination port number used by program */
#define UDP_PORT_NUM 5000

#define MAX_SOURCE_IPS 20

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
    /* The index of the interface the message was received from, or -1 if not known */
    int ifa_index;
    /* The total number of messages received from the source */
    uint64_t num_messages_received;
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
    /* The index for ifa_name */
    int ifa_index;
    /* The socket used to transmit or receive, which is bound to the interface for source_addr */
    int socket;
    /* Used to send tx_message via sendmsg() */
    struct iovec tx_iovec[1];
    /* Space used to create the tx_pktinfo */
    char tx_msg_control[1024];
    /* Used to set the source and destination IP address for transmitted messages */
    struct msghdr tx_msg;
    /* Used to transmit messages from the socket */
    test_msg_t tx_message;
    /* The number of messages sent with a successful sendto() return */
    uint32_t num_messages_sent;
    /* The number of tried to sent a message, but sendto() returned a failure */
    uint32_t num_failed_sends;
    /* Used to receive messages from the socket */
    /* The errno for the last failed send which occurred */
    int last_failed_send_errno;
    /* Used to receive rx_message via recv_msg() */
    struct iovec rx_iovec[1];
    /* Space used to read the rx_pktinfo */
    char rx_msg_control[1024];
    /* Used to obtain the source interface and IP address for received messages */
    struct msghdr rx_msg;
    test_msg_t rx_message;
    /* The number of different source addresses from which messages were received.
     * The number of valid entries in the messages_per_source[] array. */
    uint32_t num_received_sources;
    messages_from_source_t messages_per_source[MAX_SOURCE_IPS];
} per_source_ip_t;


/* The source IP tested by this program, the number set from command line arguments */
static uint32_t num_source_ips;
static per_source_ip_t source_ips[MAX_SOURCE_IPS];


/* Used to obtain the ethtool statistics for one network interface */
typedef struct interface_statistics_s
{
    /* Used to create a linked list */
    struct interface_statistics_s *next;
    /* Socket used for ioctl */
    int fd;
    /* Interface request structure used for ioctl */
    struct ifreq ifr;
    /* The number of statistics for the interface */
    uint32_t num_statistics;
    /* Names of the interface statistics */
    struct ethtool_gstrings *statistic_names;
    /* The statistic values before and after the test, so can see the number of broadcast packets during the test */
    struct ethtool_stats *statistics_before;
    struct ethtool_stats *statistics_after;
} interface_statistics_t;


/** Command line option used to specify if to use the SO_BINDTODEVICE option */
static int arg_bind_to_device = false;


/** The command line options for this program, in the format passed to getopt_long().
 *  Only long arguments are supported */
static const struct option command_line_options[] =
{
    {"bind-to-device", no_argument, &arg_bind_to_device, true},
    {NULL, 0, NULL, 0}
};


/* Set from a signal handler to request the test is stopped */
static volatile bool stop_transmission;

/**
 * @brief Signal handler to request transmission of messages is stopped
 */
static void stop_transmission_handler (const int sig)
{
    stop_transmission = true;
}


/**
 * @brief Display usage information for the program
 * @param[in] program_name Name of the program from argv[0]
 */
static void display_usage (const char *const program_name)
{
    printf ("Usage %s : <source_IPv4_1> [ ... <source_IPv4_N> ] [--bind-to-device]\n", program_name);
    printf ("  <source_IPv4> is one source IPv4 dotted-decimal address to send\n");
    printf ("  broadcast packets from.\n");
    printf ("\n");
    printf ("  --bind-to-device If this option is specified the SO_BINDTODEVICE socket\n");
    printf ("    option is used to bind to only receive packets from the interface the\n");
    printf ("    the source IP address is on. Needs capability CAP_NET_RAW\n");

    exit (EXIT_FAILURE);
}


/**
 * @brief Process one source IP address specified as a command line argument, adding to the array to be tested
 * @param[in] ip_addr_string The dotted-decimal IPv4 source address command line argument
 * @param[in] interface_addresses The list of all network interfaces on the local host
 */
static void process_source_ip_argument (const char *const ip_addr_string, struct ifaddrs *const interface_addresses)
{
    per_source_ip_t *const source_ip = &source_ips[num_source_ips];
    int rc;

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

    source_ip->ifa_index = if_nametoindex (source_ip->ifa_name);
    if (source_ip->ifa_index == 0)
    {
        printf ("Failed to get the interface index for %s\n", source_ip->ifa_name);
        exit (EXIT_FAILURE);
    }

    num_source_ips++;
}


/**
 * @brief Parse command line arguments, storing the result in global variables
 * @details Aborts the program if invalid arguments
 * @param[in] argc, argv Arguments passed to main
 * @param[in] interface_addresses The list of all network interfaces on the local host
 */
static void read_command_line_arguments (const int argc, char *argv[], struct ifaddrs *const interface_addresses)
{
    const char *const program_name = argv[0];
    int opt_status;

    /* Process any command line option flags */
    do
    {
        int option_index = 0;

        opt_status = getopt_long (argc, argv, "", command_line_options, &option_index);
        if (opt_status == '?')
        {
            display_usage (program_name);
        }
        else if (opt_status >= 0)
        {
            const struct option *const optdef = &command_line_options[option_index];

            if (optdef->flag != NULL)
            {
                /* Argument just sets a flag */
            }
            else
            {
                /* This is a program error, and shouldn't be triggered by the command line options */
                fprintf (stderr, "Unexpected argument definition %s\n", optdef->name);
                exit (EXIT_FAILURE);
            }
        }
    } while (opt_status != -1);

    /* Process the non-argument options as source IPv4 addresses */
    for (int arg_index = optind; arg_index < argc; arg_index++)
    {
        process_source_ip_argument (argv[arg_index], interface_addresses);
    }

    if (num_source_ips == 0)
    {
        printf ("At least one source IPv4 address must be specified\n");
        display_usage (program_name);
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Get the current values of the statistics for one interface
 * @param[in/out] if_stats Which interface to get the statistics for
 * @param[in] before Selects if to save the statistics for before (true) or after (false) the test.
 */
static void get_interface_statistics (interface_statistics_t *const if_stats, const bool before)
{
    if (if_stats->num_statistics > 0)
    {
        struct ethtool_stats *const stats_buf = before ? if_stats->statistics_before : if_stats->statistics_after;
        int rc;

        stats_buf->cmd = ETHTOOL_GSTATS;
        stats_buf->n_stats = if_stats->num_statistics;
        if_stats->ifr.ifr_data = (caddr_t) stats_buf;
        rc = ioctl (if_stats->fd, SIOCETHTOOL, &if_stats->ifr);
        if (rc != 0)
        {
            printf ("ETHTOOL_GSTATS failed for %s\n", if_stats->ifr.ifr_name);
            exit (EXIT_FAILURE);
        }
    }
}


/**
 * @brief Initialise the structure used to collect the statistics for one network interface.
 * @param[in] ifa_name The name of the interface to collect the statistics for
 * @return The initialised structure
 */
static interface_statistics_t *initialise_interface_statistics (const char *const ifa_name)
{
    struct
    {
        struct ethtool_sset_info hdr;
        uint32_t buf[1];
    } sset_info;
    int rc;

    interface_statistics_t *const if_stats = calloc (1, sizeof (*if_stats));

    /* Open the socket used for ioctl's */
    if_stats->fd = socket (AF_INET, SOCK_DGRAM, 0);
    if (if_stats->fd == -1)
    {
        printf ("socket() failed\n");
        exit (EXIT_FAILURE);
    }

    /* Get the number of statistics supported by the network interface.
     * May be zero on virtual interfaces such as "lo". */
    sset_info.hdr.cmd = ETHTOOL_GSSET_INFO;
    sset_info.hdr.reserved = 0;
    sset_info.hdr.sset_mask = 1ULL << ETH_SS_STATS;

    snprintf (if_stats->ifr.ifr_name, sizeof (if_stats->ifr.ifr_name), "%s", ifa_name);
    if_stats->ifr.ifr_data = (caddr_t) &sset_info;
    rc = ioctl (if_stats->fd, SIOCETHTOOL, &if_stats->ifr);
    if (rc != 0)
    {
        printf ("ETHTOOL_GSSET_INFO failed for %s\n", ifa_name);
        exit (EXIT_FAILURE);
    }

    if_stats->num_statistics = (sset_info.hdr.sset_mask != 0) ? sset_info.hdr.data[0] :0;

    if (if_stats->num_statistics > 0)
    {
        /* Get the names of the interface statistics */
        const size_t strings_len = sizeof (*if_stats->statistic_names) + (if_stats->num_statistics * ETH_GSTRING_LEN);
        if_stats->statistic_names = calloc (1, strings_len);
        if (if_stats->statistic_names == NULL)
        {
            printf ("Failed to allocate %zu bytes for statistics_names\n", strings_len);
            exit (EXIT_FAILURE);
        }

        if_stats->statistic_names->cmd = ETHTOOL_GSTRINGS;
        if_stats->statistic_names->string_set = ETH_SS_STATS;
        if_stats->statistic_names->len = if_stats->num_statistics;
        if_stats->ifr.ifr_data = (caddr_t) if_stats->statistic_names;
        rc = ioctl (if_stats->fd, SIOCETHTOOL, &if_stats->ifr);
        if (rc != 0)
        {
            printf ("ETHTOOL_GSTRINGS failed for %s\n", ifa_name);
            exit (EXIT_FAILURE);
        }

        /* Allocate space to store the statistic values */
        const size_t stats_len = sizeof (struct ethtool_stats) + (if_stats->num_statistics * sizeof (uint64_t));

        if_stats->statistics_before = calloc (1, stats_len);
        if_stats->statistics_after = calloc (1, stats_len);
        if ((if_stats->statistics_before == NULL) || (if_stats->statistics_after == NULL))
        {
            printf ("Failed to allocate %zu bytes for statistic values\n", stats_len);
            exit (EXIT_FAILURE);
        }
    }

    return if_stats;
}


/**
 * @brief Get the statistics for all network interfaces before the start of a test
 * @param[in] interface_addresses The list of all network interfaces on the local host
 * @return Returns the head of a linked list containing the statistics for all network interfaces
 */
static interface_statistics_t *get_interface_statistics_before_test (struct ifaddrs *const interface_addresses)
{
    struct ifaddrs *iap = interface_addresses;
    interface_statistics_t *head = NULL;
    interface_statistics_t *tail = NULL;

    while (iap != NULL)
    {
        /* The struct ifaddrs link-list can have the same interface multiple times, once for each IP address.
         * Therefore, add each interface to the statistics linked-list only once */
        bool ifa_name_already_found = false;
        for (const interface_statistics_t *existing_if = head;
                (existing_if != NULL) && (!ifa_name_already_found);
                existing_if = existing_if->next)
        {
            if (strcmp (iap->ifa_name, existing_if->ifr.ifr_name) == 0)
            {
                ifa_name_already_found = true;
            }
        }

        if (!ifa_name_already_found)
        {
            interface_statistics_t *const if_stats = initialise_interface_statistics (iap->ifa_name);

            get_interface_statistics (if_stats, true);
            if (head == NULL)
            {
                head = if_stats;
            }
            if (tail != NULL)
            {
                tail->next = if_stats;
            }
            tail = if_stats;
            if_stats->next = NULL;
        }

        iap = iap->ifa_next;
    }

    return head;
}


/**
 * @brief Get the statistics for all network interfaces after completion of a test
 * @param[in/out] head The linked list of all interfaces to get the statistics for
 */
static void get_interface_statistics_after_test (interface_statistics_t *const head)
{
    interface_statistics_t *if_stats = head;

    while (if_stats != NULL)
    {
        get_interface_statistics (if_stats, false);
        if_stats = if_stats->next;
    }
}


/**
 * @brief Display the change in broadcast packet statistics for all interfaces on the host during the test
 * @param[in] head The link-listed of statistics for all interfaces on the host
 */
static void display_broadcast_interface_statistics (const interface_statistics_t *const head)
{
    const interface_statistics_t *if_stats = head;

    /* Prefixes of ethtool statistic names which report the numbers of broadcast packets.
     * Taken from a sample of network interfaces. */
    static const char *const broadcast_statistic_prefixes[] =
    {
        "rx_broadcast",
        "tx_broadcast",
		"broadcast"
    };

    while (if_stats != NULL)
    {
        bool interface_header_output = false;

        for (uint32_t stats_index = 0; stats_index < if_stats->num_statistics; stats_index++)
        {
            const char *const statistic_name = (const char *) &if_stats->statistic_names->data[stats_index * ETH_GSTRING_LEN];

            for (uint32_t prefix_index = 0;
                    prefix_index < (sizeof (broadcast_statistic_prefixes) / sizeof (broadcast_statistic_prefixes[0]));
                    prefix_index++)
            {
                const char *const prefix = broadcast_statistic_prefixes[prefix_index];

                if (strncmp (statistic_name, prefix, strlen (prefix)) == 0)
                {
                    const uint64_t count_before = if_stats->statistics_before->data[stats_index];
                    const uint64_t count_after = if_stats->statistics_after->data[stats_index];

                    if (!interface_header_output)
                    {
                        printf ("Changes to broadcast statistics for interface %s during test:\n", if_stats->ifr.ifr_name);
                        interface_header_output = true;
                    }
                    printf ("  %s : %" PRIu64 " (%" PRIu64 " to %" PRIu64 ")\n",
                            statistic_name, count_after - count_before, count_before, count_after);
                }
            }
        }

        if (interface_header_output)
        {
            printf ("\n");
        }

        if_stats = if_stats->next;
    }
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
    size_t cmsg_space;
    struct cmsghdr *cmsg;
    struct in_pktinfo *tx_pktinfo;
    struct pollfd poll_fds[MAX_SOURCE_IPS] = {0};

    /* Get all interfaces to be able to lookup interface for source IP address, and to get interface statistics */
    struct ifaddrs *interface_addresses = NULL;
    rc = getifaddrs (&interface_addresses);
    if (rc != 0)
    {
        printf ("getifaddrs() failed\n");
        return EXIT_FAILURE;
    }

    /* Parse command line arguments */
    read_command_line_arguments (argc, argv, interface_addresses);

    /* Broadcast IPv4 address */
    struct sockaddr_in broadcast_addr =
    {
        .sin_family = AF_INET,
        .sin_port = htons (UDP_PORT_NUM),
        .sin_addr.s_addr = INADDR_BROADCAST
    };

    interface_statistics_t *const if_stats_head = get_interface_statistics_before_test (interface_addresses);

    /* Initialise the sockets used for each source IP address */
    for (ip_index = 0; ip_index < num_source_ips; ip_index++)
    {
        const int on = 1;
        per_source_ip_t *const source_ip = &source_ips[ip_index];

        /* Create a non-blocking UDP socket for transmit/receive */
        source_ip->socket = socket (AF_INET, SOCK_DGRAM | SOCK_NONBLOCK, 0);
        if (source_ip->socket == -1)
        {
            printf ("socket() failed\n");
            exit (EXIT_FAILURE);
        }
        poll_fds[ip_index].fd = source_ip->socket;
        poll_fds[ip_index].events = POLLIN;

        /* Allow the port to be used in case are either:
         * a. Using multiple source IP addresses on the same interface.
         * b. Not using SO_BINDTODEVICE. */
        rc = setsockopt (source_ip->socket, SOL_SOCKET, SO_REUSEPORT, &on, sizeof (on));
        if (rc != 0)
        {
            printf ("Failed to reuse port for socket\n");
            exit (EXIT_FAILURE);
        }

        /* Enable receipt of ancillary information which gives the interface on which the packet arrived */
        rc = setsockopt (source_ip->socket, IPPROTO_IP, IP_PKTINFO, &on, sizeof (on));
        if (rc != 0)
        {
            printf ("Failed to set IP_PKTINFO for socket\n");
            exit (EXIT_FAILURE);
        }

        /* When requested by command line arguments bind the socket to the device to receive broadcasts only delivered
         * to the interface.
         * SO_BINDTODEVICE requires CAP_NET_RAW.
         *
         * Without this, broadcasts received on other interfaces may be passed to the receive socket, depending upon
         * the effect of net.ipv4.conf.all.rp_filter
         *
         * Regardless of using this option, the program receives a copy of the transmitted packet on the socket
         * which sent the message. */
        if (arg_bind_to_device)
        {
            errno = 0;
            rc = setsockopt (source_ip->socket, SOL_SOCKET, SO_BINDTODEVICE, source_ip->ifa_name, sizeof (source_ip->ifa_name));
            saved_errno = errno;
            if (rc != 0)
            {
                printf ("Failed to bind to device %s : %s\n", source_ip->ifa_name, strerror (saved_errno));
                exit (EXIT_FAILURE);
            }
        }

        /* Bind to the specified fixed port number for transmit/receive */
        struct sockaddr_in rx_addr =
        {
            .sin_family = AF_INET,
            .sin_port = htons (UDP_PORT_NUM),
        };
        errno = 0;
        rc = bind (source_ip->socket, &rx_addr, sizeof (rx_addr));
        saved_errno = errno;
        if (rc != 0)
        {
            printf ("Failed to bind socket to %s : %s\n", source_ip->ip_addr_string, strerror (saved_errno));
            exit (EXIT_FAILURE);
        }

        /* Allow broadcast on the transmit socket */
        int enable_broadcast = 1;
        rc = setsockopt (source_ip->socket, SOL_SOCKET, SO_BROADCAST, &enable_broadcast, sizeof (enable_broadcast));
        if (rc != 0)
        {
            printf ("setsockopt (SO_BROADCAST) failed\n");
            exit (EXIT_FAILURE);
        }

        /* Create the IO vector for the single transmit message buffer */
        source_ip->tx_iovec[0].iov_base = &source_ip->tx_message;
        source_ip->tx_iovec[0].iov_len = sizeof (source_ip->tx_message);

        /* Set the pktinfo structure such that the source IP address is set */
        source_ip->tx_msg.msg_name = &broadcast_addr;
        source_ip->tx_msg.msg_namelen = sizeof (broadcast_addr);
        source_ip->tx_msg.msg_iov = source_ip->tx_iovec;
        source_ip->tx_msg.msg_iovlen = sizeof (source_ip->tx_iovec) / sizeof (source_ip->tx_iovec[0]);
        source_ip->tx_msg.msg_control = source_ip->tx_msg_control;
        source_ip->tx_msg.msg_controllen = sizeof (source_ip->tx_msg_control);
        source_ip->tx_msg.msg_flags = 0;
        cmsg_space = 0;
        cmsg = CMSG_FIRSTHDR (&source_ip->tx_msg);
        cmsg->cmsg_level = IPPROTO_IP;
        cmsg->cmsg_type = IP_PKTINFO;
        cmsg->cmsg_len = CMSG_LEN (sizeof (*tx_pktinfo));
        tx_pktinfo = (struct in_pktinfo *) CMSG_DATA (cmsg);
        tx_pktinfo->ipi_ifindex = source_ip->ifa_index;
        tx_pktinfo->ipi_spec_dst = source_ip->source_addr;
        cmsg_space += CMSG_SPACE (sizeof (*tx_pktinfo));
        source_ip->tx_msg.msg_controllen = cmsg_space;

        source_ip->num_messages_sent = 0;
        source_ip->num_failed_sends = 0;
        source_ip->tx_message.sequence_number = 0;
        snprintf (source_ip->tx_message.source_ip_addr_string, sizeof (source_ip->tx_message.source_ip_addr_string), "%s",
                source_ip->ip_addr_string);
        source_ip->num_received_sources = 0;
    }

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

                errno = 0;
                rc = sendmsg (source_ip->socket, &source_ip->tx_msg, 0);
                saved_errno = errno;
                if (rc == sizeof (source_ip->tx_message))
                {
                    source_ip->num_messages_sent++;
                }
                else
                {
                    source_ip->num_failed_sends++;
                    source_ip->last_failed_send_errno = saved_errno;
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

            if ((poll_fds[ip_index].revents & POLLIN) == POLLIN)
            {
                struct sockaddr_in from = {0};
                struct iovec rx_iovec[1];

                /* Initialise the structure which allows the ancillary information with the source interface to be obtained */
                rx_iovec[0].iov_base = &source_ip->rx_message;
                rx_iovec[0].iov_len = sizeof (source_ip->rx_message);
                source_ip->rx_msg.msg_iov = rx_iovec;
                source_ip->rx_msg.msg_iovlen = sizeof (rx_iovec) / sizeof (rx_iovec[0]);
                source_ip->rx_msg.msg_name = &from;
                source_ip->rx_msg.msg_namelen = sizeof (from);
                source_ip->rx_msg.msg_control = source_ip->rx_msg_control;
                source_ip->rx_msg.msg_controllen = sizeof (source_ip->rx_msg_control);
                source_ip->rx_msg.msg_flags = 0;

                rc = recvmsg (source_ip->socket, &source_ip->rx_msg, 0);
                if ((rc == sizeof (source_ip->rx_message)) && (from.sin_family == AF_INET))
                {
                    bool found_existing_source = false;

                    /* Extract the source interface from the ancillary information */
                    int ifa_index = -1;
                    for (cmsg = CMSG_FIRSTHDR (&source_ip->rx_msg); cmsg != NULL; cmsg = CMSG_NXTHDR (&source_ip->rx_msg, cmsg))
                    {
                        if ((cmsg->cmsg_level == IPPROTO_IP) && (cmsg->cmsg_type == IP_PKTINFO))
                        {
                            const struct in_pktinfo *const rx_pktinfo = (const struct in_pktinfo *) CMSG_DATA (cmsg);

                            ifa_index = rx_pktinfo->ipi_ifindex;
                        }
                    }

                    /* Search for an existing combination of source IP address and source interface index */
                    for (uint32_t rx_source_index = 0;
                            (!found_existing_source) && (rx_source_index < source_ip->num_received_sources);
                            rx_source_index++)
                    {
                        messages_from_source_t *const existing_source = &source_ip->messages_per_source[rx_source_index];

                        if ((from.sin_addr.s_addr == existing_source->s_addr) && (ifa_index == existing_source->ifa_index))
                        {
                            existing_source->num_messages_received++;
                            existing_source->last_message = source_ip->rx_message;
                            found_existing_source = true;
                        }
                    }

                    if ((!found_existing_source) && (source_ip->num_received_sources < MAX_SOURCE_IPS))
                    {
                        messages_from_source_t *const new_source = &source_ip->messages_per_source[source_ip->num_received_sources];

                        new_source->ifa_index = ifa_index;
                        new_source->s_addr = from.sin_addr.s_addr;
                        new_source->num_messages_received = 1;
                        new_source->first_message = source_ip->rx_message;
                        new_source->last_message = source_ip->rx_message;
                        source_ip->num_received_sources++;
                    }
                }
            }
        }
    }

    get_interface_statistics_after_test (if_stats_head);

    /* Display the results of the test */
    const int64_t test_end_time = get_monotonic_time ();
    const double test_duration_seconds = (double) (test_end_time - test_start_time) / 1E9;

    printf ("\nTest ran for %.3f seconds\n", test_duration_seconds);
    for (ip_index = 0; ip_index < num_source_ips; ip_index++)
    {
        const per_source_ip_t *const source_ip = &source_ips[ip_index];

        printf ("Results for source IP %s on interface %s\n", source_ip->ip_addr_string, source_ip->ifa_name);
        printf ("  num_messages_sent=%" PRIu32 "  num_failed_sends=%" PRIu32 "\n",
                source_ip->num_messages_sent, source_ip->num_failed_sends);
        if (source_ip->num_failed_sends > 0)
        {
            printf ("  last failed send error : %s\n", strerror (source_ip->last_failed_send_errno));
        }
        printf ("  num_received_sources=%" PRIu32 "\n", source_ip->num_received_sources);
        for (uint32_t rx_source_index = 0; rx_source_index < source_ip->num_received_sources; rx_source_index++)
        {
            char ip_addr_string[INET6_ADDRSTRLEN];
            char interface_name[IF_NAMESIZE];
            char *if_index_to_name_status = NULL;
            const char *inet_ntop_status;
            const messages_from_source_t *const rx_source = &source_ip->messages_per_source[rx_source_index];
            const uint64_t rx_sequence_num_range =
                    (rx_source->last_message.sequence_number - rx_source->first_message.sequence_number) + 1;

            if (rx_source->ifa_index != 1)
            {
                if_index_to_name_status = if_indextoname (rx_source->ifa_index, interface_name);
            }

            inet_ntop_status = inet_ntop (AF_INET, &rx_source->s_addr, ip_addr_string, sizeof (ip_addr_string));
            if (inet_ntop_status == NULL)
            {
                snprintf (ip_addr_string, sizeof (ip_addr_string), "%s", "<inet_ntop() failed>");
            }
            printf ("  [%" PRIu32 "] source interface=%s\n",
                    rx_source_index, (if_index_to_name_status != NULL) ? interface_name : "<unknown>");
            printf ("  [%" PRIu32 "] source_from_recvmsg=%s  source_from_message=%s\n",
                    rx_source_index, ip_addr_string, rx_source->first_message.source_ip_addr_string);
            printf ("  [%" PRIu32 "] num_received=%" PRIu64 "  rx_sequence_nums=%" PRIu64 "..%" PRIu64 " (%" PRIu64 " missed)\n",
                    rx_source_index, rx_source->num_messages_received,
                    rx_source->first_message.sequence_number, rx_source->last_message.sequence_number,
                    rx_sequence_num_range - rx_source->num_messages_received);
        }
        printf ("\n");
    }

    display_broadcast_interface_statistics (if_stats_head);

    return EXIT_SUCCESS;
}
