/*
 * @file ibv_raw_packet_tx.c
 * @date 3 Jan 2022
 * @author Chester Gillon
 * @details A variant of the https://github.com/chester-gillon/switch_test but which uses a RDMA IBV_QPT_RAW_PACKET to
 *          transmit the frames, rather than PCAP.
 *
 *          This is currently transmit only, since the ConnectX-2 used doesn't support receiving raw Ethernet frames
 *          when using RDMA. Attempting to use the example code from
 *          https://community.mellanox.com/s/article/raw-ethernet-programming--basic-introduction---code-example
 *          caused the ibv_create_flow() call to fail with EOPNOTSUPP "Operation not supported".
 *
 *          In the absence of the program being able to receive and check the frames, a switch monitor session was
 *          used to copy the frames to a different PC at which Wireshark was used to manually inspect that valid
 *          EtherCAT frames were transmitted, with the expected VLAN tags.
 *
 *          man ibv_create_flow has the note:
 *             1. These verbs are available only for devices supporting IBV_DEVICE_MANAGED_FLOW_STEERING and only for
 *                QPs of Transport Service Type IBV_QPT_UD or IBV_QPT_RAW_PACKET
 *
 *          Where ibv_devinfo shows a ConnectX-2 VPI set to Ethernet link-layer doesn't have
 *          IBV_DEVICE_MANAGED_FLOW_STEERING.
 *
 *          The dmesg output from the ConnectX-2 driver reports:
 *             Steering mode is: B0 steering, oper_log_mgm_entry_size = 10, modparam log_num_mgm_entry_size = 10
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <time.h>

#include <unistd.h>
#include <arpa/inet.h>

#include <infiniband/verbs.h>


#define NSECS_PER_SEC 1000000000LL

/* Ethernet frame types */
#define ETH_P_8021Q    0x8100
#define ETH_P_ETHERCAT 0x88a4

/* Maximum number of sends waiting for completion */
#define SQ_NUM_DESC 512

#define ETHER_MAC_ADDRESS_LEN 6

/** Defines the unique identity used for one switch port under test.
 *  The MAC address is used by the switch under test to route traffic to the expected port.
 *  The VLAN is used by the injection switch.
 *
 *  The switch_port_number is that of the switch under test, controlled by the cabling and the VLAN assignment,
 *  i.e. for information and not set by the software.
 */
typedef struct
{
    uint32_t switch_port_number;
    uint8_t mac_addr[ETHER_MAC_ADDRESS_LEN];
    uint16_t vlan;
} port_id_t;


/** Define a locally administated MAC address and VLAN for each possible switch port under test.
    The last octet of the MAC address is the index into the array, which is used an optimisation when looking
    up the port number of a received frame (to avoid having to search the table). */
#define NUM_DEFINED_PORTS 48
static const port_id_t test_ports[NUM_DEFINED_PORTS] =
{
    { .switch_port_number =  1, .mac_addr = {2,0,1,0,0, 0}, .vlan = 1001},
    { .switch_port_number =  2, .mac_addr = {2,0,1,0,0, 1}, .vlan = 1002},
    { .switch_port_number =  3, .mac_addr = {2,0,1,0,0, 2}, .vlan = 1003},
    { .switch_port_number =  4, .mac_addr = {2,0,1,0,0, 3}, .vlan = 1004},
    { .switch_port_number =  5, .mac_addr = {2,0,1,0,0, 4}, .vlan = 1005},
    { .switch_port_number =  6, .mac_addr = {2,0,1,0,0, 5}, .vlan = 1006},
    { .switch_port_number =  7, .mac_addr = {2,0,1,0,0, 6}, .vlan = 1007},
    { .switch_port_number =  8, .mac_addr = {2,0,1,0,0, 7}, .vlan = 1008},
    { .switch_port_number =  9, .mac_addr = {2,0,1,0,0, 8}, .vlan = 1009},
    { .switch_port_number = 10, .mac_addr = {2,0,1,0,0, 9}, .vlan = 1010},
    { .switch_port_number = 11, .mac_addr = {2,0,1,0,0,10}, .vlan = 1011},
    { .switch_port_number = 12, .mac_addr = {2,0,1,0,0,11}, .vlan = 1012},
    { .switch_port_number = 13, .mac_addr = {2,0,1,0,0,12}, .vlan = 1013},
    { .switch_port_number = 14, .mac_addr = {2,0,1,0,0,13}, .vlan = 1014},
    { .switch_port_number = 15, .mac_addr = {2,0,1,0,0,14}, .vlan = 1015},
    { .switch_port_number = 16, .mac_addr = {2,0,1,0,0,15}, .vlan = 1016},
    { .switch_port_number = 17, .mac_addr = {2,0,1,0,0,16}, .vlan = 1017},
    { .switch_port_number = 18, .mac_addr = {2,0,1,0,0,17}, .vlan = 1018},
    { .switch_port_number = 19, .mac_addr = {2,0,1,0,0,18}, .vlan = 1019},
    { .switch_port_number = 20, .mac_addr = {2,0,1,0,0,19}, .vlan = 1020},
    { .switch_port_number = 21, .mac_addr = {2,0,1,0,0,20}, .vlan = 1021},
    { .switch_port_number = 22, .mac_addr = {2,0,1,0,0,21}, .vlan = 1022},
    { .switch_port_number = 23, .mac_addr = {2,0,1,0,0,22}, .vlan = 1023},
    { .switch_port_number = 24, .mac_addr = {2,0,1,0,0,23}, .vlan = 1024},
    { .switch_port_number = 25, .mac_addr = {2,0,1,0,0,24}, .vlan = 1025},
    { .switch_port_number = 26, .mac_addr = {2,0,1,0,0,25}, .vlan = 1026},
    { .switch_port_number = 27, .mac_addr = {2,0,1,0,0,26}, .vlan = 1027},
    { .switch_port_number = 28, .mac_addr = {2,0,1,0,0,27}, .vlan = 1028},
    { .switch_port_number = 29, .mac_addr = {2,0,1,0,0,28}, .vlan = 1029},
    { .switch_port_number = 30, .mac_addr = {2,0,1,0,0,29}, .vlan = 1030},
    { .switch_port_number = 31, .mac_addr = {2,0,1,0,0,30}, .vlan = 1031},
    { .switch_port_number = 32, .mac_addr = {2,0,1,0,0,31}, .vlan = 1032},
    { .switch_port_number = 33, .mac_addr = {2,0,1,0,0,32}, .vlan = 1033},
    { .switch_port_number = 34, .mac_addr = {2,0,1,0,0,33}, .vlan = 1034},
    { .switch_port_number = 35, .mac_addr = {2,0,1,0,0,34}, .vlan = 1035},
    { .switch_port_number = 36, .mac_addr = {2,0,1,0,0,35}, .vlan = 1036},
    { .switch_port_number = 37, .mac_addr = {2,0,1,0,0,36}, .vlan = 1037},
    { .switch_port_number = 38, .mac_addr = {2,0,1,0,0,37}, .vlan = 1038},
    { .switch_port_number = 39, .mac_addr = {2,0,1,0,0,38}, .vlan = 1039},
    { .switch_port_number = 40, .mac_addr = {2,0,1,0,0,39}, .vlan = 1040},
    { .switch_port_number = 41, .mac_addr = {2,0,1,0,0,40}, .vlan = 1041},
    { .switch_port_number = 42, .mac_addr = {2,0,1,0,0,41}, .vlan = 1042},
    { .switch_port_number = 43, .mac_addr = {2,0,1,0,0,42}, .vlan = 1043},
    { .switch_port_number = 44, .mac_addr = {2,0,1,0,0,43}, .vlan = 1044},
    { .switch_port_number = 45, .mac_addr = {2,0,1,0,0,44}, .vlan = 1045},
    { .switch_port_number = 46, .mac_addr = {2,0,1,0,0,45}, .vlan = 1046},
    { .switch_port_number = 47, .mac_addr = {2,0,1,0,0,46}, .vlan = 1047},
    { .switch_port_number = 48, .mac_addr = {2,0,1,0,0,47}, .vlan = 1048},
};


/* Array which defines which of the indices in test_ports[] are tested at run-time.
 * This allows command line arguments to specify a sub-set of the possible ports to be tested. */
static uint32_t tested_port_indices[NUM_DEFINED_PORTS];
static uint32_t num_tested_port_indices;


/* Command line arguments which specify the RDMA device and port used to send test frames */
static char arg_rdma_device_name[PATH_MAX];
static uint8_t arg_rdma_port_num;


/* Command line argument which specifies the test interval in seconds, which is the interval over which statistics are
 * accumulated and then reported. */
static int64_t arg_test_interval_secs = 10;


/* Command line argument which specifies the maximum rate at which transmit frames can be sent, by the software.
 * If <= 0 then no maximum rate is applied by the software, i.e. transmits as quickly as possible. */
static int64_t arg_max_frame_rate_hz = -1;
static bool arg_max_frame_rate_hz_set = false;


/* Command line argument which specifies the rate limit set on the Queue-Pair */
static uint32_t arg_qp_rate_limit_kbps = 0;
static bool arg_qp_rate_limit_kbps_set = false;


/* Command line argument which specifies ibv_modify_qp_rate_limit() is used to set the rate limit,
 * to also control the burst size. */
static bool arg_qp_limit_burst_rate = false;


/**
 * Defines the layout of one maximum length Ethernet frame, with a single EtherCAT datagram, for the test.
 * EtherCAT has it own EtherType which can be used to filter the received frames to only those frames.
 *
 * Layout taken from:
 *   https://www.szcomark.com/info/ethercat-frame-structure-59044613.html
 *   https://infosys.beckhoff.com/english.php?content=../content/1033/tc3_io_intro/1257993099.html
 *
 * The Address is used a sequence number incremented for each test frame transmitted.
 *
 * The value of ETHERCAT_DATAGRAM_LEN results in the maximum MTU of 1500, which is formed from the EtherCAT Datagram header
 * through Working Counter inclusive.
 *
 * https://en.wikipedia.org/wiki/Ethernet_frame notes that the IEEE 802.3ac specification which added the
 * VLAN tag increased the maximum frame size by 4 octets to allow for the encapsulated VLAN tag.
 *
 * One difference between Linux and Windows found that with the MTU set to 1500:
 * a. Linux could send this frame without any configuration changes.
 * b. Under Windows attempting to send this frame resulted in an error from the PacketSendPacket() function in the
 *    PACKET.DLL packet capture driver which the pcap_sendpacket() PCAP function calls.
 *    To allow this frame to be sent under Windows had use the Windows device manager for the network adapter to
 *    enable the "Jumbo Frame" or "Jumbo Packet" option, selecting the smallest jumbo frame size.
 */
#define ETHERCAT_DATAGRAM_LEN 1486
typedef struct __attribute__((packed))
{
    /* Ethernet Header */
    uint8_t destination_mac_addr[ETHER_MAC_ADDRESS_LEN];
    uint8_t source_mac_addr[ETHER_MAC_ADDRESS_LEN];
    uint16_t ether_type; /* Set to indicate a VLAN */

    /* VLAN id */
    uint16_t vlan_tci;

    uint16_t vlan_ether_type;

    /* EtherCAT header */
    uint16_t Length:11;  /* Length of the EtherCAT datagram (without FCS) */
    uint16_t Reserved:1; /* Reserved, 0 */
    uint16_t Type:4;     /* Protocol type. EtherCAT slave controllers (ESCs) only support EtherCAT commands (type = 0x1). */

    /* EtherCAT Datagram header */
    uint8_t Cmd; /* EtherCAT command type */
    uint8_t Idx; /* The index is a numerical identifier used by the master to identify duplicates or lost
                  * datagrams. The EtherCAT slaves should not change the index. */
    uint32_t Address; /* Address: auto-increment, configured station address or logical address */
    uint16_t Len:11;  /* Length of the data following within this datagram */
    uint16_t R:3;     /* Reserved, 0 */
    uint16_t C:1;     /* Circulating frame:
                       * 0: Frame does not circulate
                       * 1: Frame has circulated once */
    uint16_t M:1;     /* Multiple EtherCAT datagrams
                       * 0: Last EtherCAT datagram
                       * 1: At least one further EtherCAT datagram follows */
    uint16_t IRQ;     /* EtherCAT event request register of all slave devices combined with a logical OR */
    uint8_t data[ETHERCAT_DATAGRAM_LEN]; /* Data to be read or written */
    uint16_t WKC;     /* Working Counter */
} ethercat_frame_t;


#define MIN_TESTED_PORTS 2


/**
 * @brief Abort the program if an assertion fails, after displaying a message
 * @param[in] assertion Should be true to allow the program to continue.
 * @param[in] format printf style format string for error message.
 * @param[in] ... printf arguments
 */
#define CHECK_ASSERT(assertion) check_assert(assertion,#assertion)
static void check_assert (const bool assertion, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
static void check_assert (const bool assertion, const char *format, ...)
{
    if (!assertion)
    {
        va_list args;

        va_start (args, format);
        fprintf (stderr, "Assertion failed : ");
        vfprintf (stderr, format, args);
        va_end (args);
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Display the program usage and then exit
 * @param[in] program_name Name of the program from argv[0]
 */
static void display_usage (const char *const program_name)
{
    printf ("Usage %s: -i <rdma_device_name> -n <rdma_port_num> [-t <duration_secs>] [-p <port_list>] [-r <rate_hz>] [-l <rate_kpbs>] [-b]\n", program_name);
    printf ("\n");
    printf ("  -i specifies the name of the RDMA device to send frames on\n");
    printf ("  -n specifies the name of the RDMA port number to send frames on\n");
    printf ("  -t defines the duration of a test interval in seconds, over which the number\n");
    printf ("     errors is accumulated and reported.\n");
    printf ("  -p defines which switch ports to test. The <port_list> is a comma separated\n");
    printf ("     string, with each item either a single port number or a start and end port\n");
    printf ("     range delimited by a dash. E.g. 1,4-6 specifies ports 1,4,5,6.\n");
    printf ("     If not specified defaults to all %u defined ports\n", NUM_DEFINED_PORTS);
    printf ("  -r Specifies the maximum rate at which frames are transmitted.\n");
    printf ("     Using a value <= 0 means the frame transmission rate is not limited in,\n");
    printf ("     software even when testing a small number of ports.\n");
    printf ("  -l Specifies the rate limit in Kb/s to set in the Queue-Pair.\n");
    printf ("  -b If specified causes ibv_modify_qp_rate_limit to set the rate limit,\n");
    printf ("     to also be able to set the burst size\n");

    exit (EXIT_FAILURE);
}


/**
 * @brief store one switch port number to be tested
 * @details Exits with an error port_num fails validation checks
 * @param[in] port_num The switch port number specified on the command line which is to be tested
 */
static void store_tested_port_num (const uint32_t port_num)
{
    bool found;
    uint32_t port_index;

    /* Check the port number is defined */
    port_index = 0;
    found = false;
    while ((!found) && (port_index < NUM_DEFINED_PORTS))
    {
        if (test_ports[port_index].switch_port_number == port_num)
        {
            found = true;
        }
        else
        {
            port_index++;
        }
    }
    if (!found)
    {
        printf ("Error: Switch port %" PRIu32 " is not defined in the compiled-in list\n", port_num);
        exit (EXIT_FAILURE);
    }

    /* Check the port number hasn't been specified more than once */
    for (uint32_t tested_port_index = 0; tested_port_index < num_tested_port_indices; tested_port_index++)
    {
        if (test_ports[tested_port_indices[tested_port_index]].switch_port_number == port_num)
        {
            printf ("Error: Switch port %" PRIu32 " specified more than once\n", port_num);
            exit (EXIT_FAILURE);
        }
    }

    tested_port_indices[num_tested_port_indices] = port_index;
    num_tested_port_indices++;
}


/*
 * @brief Parse the list of switch ports to be tested, storing in tested_port_indicies[]
 * @details Exits with an error if the port list isn't valid.
 * @param[in] tested_port_indicies The string contain the list of ports from the command line
 */
static void parse_tested_port_list (const char *const port_list_in)
{
    const char *const ports_delim = ",";
    const char *const range_delim = "-";
    char *const port_list = strdup (port_list_in);
    char *port_list_saveptr = NULL;
    char *port_range_saveptr = NULL;
    char *port_list_token;
    char *port_range_token;
    uint32_t port_num;
    char junk;
    uint32_t range_start_port_num;
    uint32_t range_end_port_num;

    /* Process the list of comma separated port numbers */
    num_tested_port_indices = 0;
    port_list_token = strtok_r (port_list, ports_delim, &port_list_saveptr);
    while (port_list_token != NULL)
    {
        /* For each comma separated item extract as either a single port number, or a range of consecutive port numbers
         * separated by a dash. */
        port_range_token = strtok_r (port_list_token, range_delim, &port_range_saveptr);
        if (sscanf (port_range_token, "%" SCNu32 "%c", &port_num, &junk) != 1)
        {
            printf ("Error: %s is not a valid port number\n", port_range_token);
            exit (EXIT_FAILURE);
        }
        range_start_port_num = port_num;

        port_range_token = strtok_r (NULL, range_delim, &port_range_saveptr);
        if (port_range_token != NULL)
        {
            /* A range of port numbers specified */
            if (sscanf (port_range_token, "%" SCNu32 "%c", &port_num, &junk) != 1)
            {
                printf ("Error: %s is not a valid port number\n", port_range_token);
                exit (EXIT_FAILURE);
            }
            range_end_port_num = port_num;

            port_range_token = strtok_r (NULL, range_delim, &port_range_saveptr);
            if (port_range_token != NULL)
            {
                printf ("Error: %s unexpected spurious port range\n", port_range_token);
                exit (EXIT_FAILURE);
            }

            if (range_end_port_num < range_start_port_num)
            {
                printf ("Error: port range end %" PRIu32 " is less than port range start %" PRIu32 "\n",
                        range_end_port_num, range_start_port_num);
                exit (EXIT_FAILURE);
            }
        }
        else
        {
            /* A single port number specified */
            range_end_port_num = port_num;
        }

        for (port_num = range_start_port_num; port_num <= range_end_port_num; port_num++)
        {
            store_tested_port_num (port_num);
        }

        port_list_token = strtok_r (NULL, ports_delim, &port_list_saveptr);
    }
}


/**
 * @brief Read the command line arguments, exiting if an error in the arguments
 * @param[in] argc, argv Command line arguments passed to main
 */
static void read_command_line_arguments (const int argc, char *argv[])
{
    const char *const program_name = argv[0];
    const char *const optstring = "i:n:t:p:r:l:b";
    bool rdma_device_specified = false;
    bool rdma_port_specified = false;
    int option;
    char junk;
    uint32_t port_num;

    /* Process the command line arguments */
    option = getopt (argc, argv, optstring);
    while (option != -1)
    {
        switch (option)
        {
        case 'i':
            snprintf (arg_rdma_device_name, sizeof (arg_rdma_device_name), "%s", optarg);
            rdma_device_specified = true;
            break;

        case 'n':
            if ((sscanf (optarg, "%" SCNu32 "%c", &port_num, &junk) != 1) || (port_num > 255))
            {
                printf ("Error: Invalid RDMA port num %s\n", optarg);
                exit (EXIT_FAILURE);
            }
            arg_rdma_port_num = (uint8_t) port_num;
            rdma_port_specified = true;
            break;

        case 't':
            if ((sscanf (optarg, "%" SCNi64 "%c", &arg_test_interval_secs, &junk) != 1) ||
                (arg_test_interval_secs <= 0))
            {
                printf ("Error: Invalid <duration_secs> %s\n", optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case 'p':
            parse_tested_port_list (optarg);
            break;

        case 'r':
            if (sscanf (optarg, "%" SCNi64 "%c", &arg_max_frame_rate_hz, &junk) != 1)
            {
                printf ("Error: Invalid <rate_hz> %s\n", optarg);
                exit (EXIT_FAILURE);
            }
            arg_max_frame_rate_hz_set = true;
            break;

        case 'l':
            if (sscanf (optarg, "%" SCNu32 "%c", &arg_qp_rate_limit_kbps, &junk) != 1)
            {
                printf ("Error: Invalid <rate_kpbs> %s\n", optarg);
                exit (EXIT_FAILURE);
            }
            arg_qp_rate_limit_kbps_set = true;
            break;

        case 'b':
            arg_qp_limit_burst_rate = true;
            break;

        case '?':
        default:
            display_usage (program_name);
            break;
        }

        option = getopt (argc, argv, optstring);
    }

    /* Check the expected arguments have been provided */
    if (!rdma_device_specified || !rdma_port_specified)
    {
        printf ("Error: The RDMA device and port must be specified\n\n");
        display_usage (program_name);
    }

    if (num_tested_port_indices < MIN_TESTED_PORTS)
    {
        printf ("Error: A minimum of %d ports must be tested\n\n", MIN_TESTED_PORTS);
        display_usage (program_name);
    }

    if (arg_qp_limit_burst_rate)
    {
        if (!arg_qp_rate_limit_kbps_set)
        {
            printf ("Error: <rate_kpbs> must be specified when limiting the burst rate\n");
            display_usage (program_name);
        }
#ifndef HAVE_IBV_MODIFY_QP_RATE_LIMIT
        printf ("Error: program hasn't been compiled with support for limiting the burst rate\n");
        display_usage (program_name);
#endif
    }

    if (optind < argc)
    {
        printf ("Error: Unexpected nonoption (first %s)\n\n", argv[optind]);
        display_usage (program_name);
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
    CHECK_ASSERT (rc == 0);

    return (now.tv_sec * NSECS_PER_SEC) + now.tv_nsec;
}


/**
 * @brief Create a EtherCAT test frame which can be sent.
 * @details The EtherCAT commands and datagram contents are not significant; have just used values which populate
 *          a maximum length frame and for which Wireshark reports a valid frame (for debugging).
 * @param frame[out] The populated frame which can be transmitted.
 * @param source_port_index[in] The index into test_ports[] which selects the source MAC address and outgoing VLAN.
 * @param destination_port_index[in] The index into test_ports[] which selects the destination MAC address.
 * @param sequence_number[in] The sequence number to place in the transmitted frame
 */
static void create_test_frame (ethercat_frame_t *const frame,
                               const uint32_t source_port_index, const uint32_t destination_port_index,
                               const uint32_t sequence_number)
{
    memset (frame, 0, sizeof (*frame));

    /* MAC addresses */
    memcpy (frame->destination_mac_addr, test_ports[destination_port_index].mac_addr,
            sizeof (frame->destination_mac_addr));
    memcpy (frame->source_mac_addr, test_ports[source_port_index].mac_addr,
            sizeof (frame->source_mac_addr));

    /* VLAN */
    frame->ether_type = htons (ETH_P_8021Q);
    frame->vlan_tci = htons (test_ports[source_port_index].vlan);

    frame->vlan_ether_type = htons (ETH_P_ETHERCAT);

    frame->Length = sizeof (ethercat_frame_t) - offsetof (ethercat_frame_t, Cmd);
    frame->Type = 1; /* EtherCAT commands */
    frame->Cmd = 11; /* Logical Memory Write */
    frame->Len = ETHERCAT_DATAGRAM_LEN;

    static uint8_t fill_value;
    for (uint32_t data_index = 0; data_index < ETHERCAT_DATAGRAM_LEN; data_index++)
    {
        frame->data[data_index] = fill_value++;
    }

    frame->Address = sequence_number;
}


int main (int argc, char *argv[])
{
    int num_ibv_devices = 0;
    struct ibv_device **device_list;
    int rc;
    int saved_errno;

    /* Check that ethercat_frame_t has the expected size */
    if (sizeof (ethercat_frame_t) != 1518)
    {
        fprintf (stderr, "sizeof (ethercat_frame_t) unexpected value of %" PRIuPTR "\n", sizeof (ethercat_frame_t));
        exit (EXIT_FAILURE);
    }

    read_command_line_arguments (argc, argv);

    /* Find all RDMA devices */
    device_list = ibv_get_device_list (&num_ibv_devices);
    check_assert (num_ibv_devices > 0, "No RDMA devices found");

    /* Open the requested RDMA device */
    struct ibv_context *rdma_device = NULL;
    for (int device_index = 0; (rdma_device == NULL) && (device_index < num_ibv_devices); device_index++)
    {
        if (strcmp (device_list[device_index]->name, arg_rdma_device_name) == 0)
        {
            rdma_device = ibv_open_device (device_list[device_index]);
            CHECK_ASSERT (rdma_device != NULL);
        }
    }

    if (rdma_device == NULL)
    {
        fprintf (stderr, "RDMA device %s not found\n", arg_rdma_device_name);
        exit (EXIT_FAILURE);
    }

    struct ibv_device_attr_ex device_attributes;
    rc = ibv_query_device_ex (rdma_device, NULL, &device_attributes);
    CHECK_ASSERT (rc == 0);
    if ((arg_rdma_port_num < 1) || (arg_rdma_port_num > device_attributes.orig_attr.phys_port_cnt))
    {
        fprintf (stderr, "RDMA port number %u outside of valid range 1..%u for device %s\n",
                arg_rdma_port_num, device_attributes.orig_attr.phys_port_cnt, arg_rdma_device_name);
        exit (EXIT_FAILURE);
    }
    printf ("HCA core clock %" PRIu64 " KHz\n", device_attributes.hca_core_clock);

    if (arg_qp_rate_limit_kbps_set)
    {
        /* Warn if a Queue-Pair rate limit was specified in the command line arguments, but the device doesn't support it */
        if (!ibv_is_qpt_supported (device_attributes.packet_pacing_caps.supported_qpts, IBV_QPT_RAW_PACKET))
        {
            printf ("Warning: QP rate limit requested, but not supported for a IBV_QPT_RAW_PACKET\n");
        }
        else if ((arg_qp_rate_limit_kbps < device_attributes.packet_pacing_caps.qp_rate_limit_min) ||
                 (arg_qp_rate_limit_kbps > device_attributes.packet_pacing_caps.qp_rate_limit_max))
        {
            printf ("Warning: QP rate limit of %" PRIu32 "Kbps outside of supported range of %" PRIu32 "-%" PRIu32 "\n",
                    arg_qp_rate_limit_kbps,
                    device_attributes.packet_pacing_caps.qp_rate_limit_min,
                    device_attributes.packet_pacing_caps.qp_rate_limit_max);
        }
    }

    /* Allocate protection domain */
    struct ibv_pd *pd;
    pd = ibv_alloc_pd (rdma_device);
    CHECK_ASSERT (pd != NULL);

    /* Create completion queue */
    struct ibv_cq *cq;
    cq = ibv_create_cq (rdma_device, SQ_NUM_DESC, NULL, NULL, 0);
    CHECK_ASSERT (cq != NULL);

    /* Allocate array, and memory region, to populate the frames which are queued for transmission */
    struct ibv_mr *mr;
    ethercat_frame_t *const tx_frames = calloc (SQ_NUM_DESC, sizeof (ethercat_frame_t));
    mr = ibv_reg_mr (pd, tx_frames, sizeof (ethercat_frame_t) * SQ_NUM_DESC, IBV_ACCESS_LOCAL_WRITE);
    CHECK_ASSERT (mr != NULL);

    /* Create Queue-Pair to send raw packets */
    struct ibv_qp *qp;
    struct ibv_qp_init_attr qp_init_attr =
    {
        .send_cq = cq,
        .recv_cq = cq,

        .cap =
        {
            /* Send uses one SGE for each possible descriptor */
            .max_send_wr = SQ_NUM_DESC,
            .max_send_sge = 1,

            /* Receive not used */
            .max_recv_wr = 0
        },

        .qp_type = IBV_QPT_RAW_PACKET
    };

    /* Create the Queue-Pair */
    errno = 0;
    qp = ibv_create_qp (pd, &qp_init_attr);
    saved_errno = errno;
    if ((qp == NULL) && (saved_errno == EPERM))
    {
        fprintf (stderr, "No permission to create raw packet - cap_net_raw capability is needed\n");
        exit (EXIT_FAILURE);
    }
    CHECK_ASSERT (qp != NULL);

    struct ibv_qp_attr qp_attr;
    int attr_mask;

    /* Transition Queue-Pair through the required states to be able to transmit */
    memset (&qp_attr, 0, sizeof(qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.port_num = arg_rdma_port_num;
    rc = ibv_modify_qp (qp, &qp_attr, IBV_QP_STATE | IBV_QP_PORT);
    CHECK_ASSERT (rc == 0);

    memset (&qp_attr, 0, sizeof(qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    rc = ibv_modify_qp (qp, &qp_attr, IBV_QP_STATE);
    CHECK_ASSERT (rc == 0);

    memset (&qp_attr, 0, sizeof(qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    attr_mask = IBV_QP_STATE;
    if (arg_qp_rate_limit_kbps_set && (!arg_qp_limit_burst_rate))
    {
        qp_attr.rate_limit = arg_qp_rate_limit_kbps;
        attr_mask |= IBV_QP_RATE_LIMIT;
    }
    rc = ibv_modify_qp (qp, &qp_attr, attr_mask);
    CHECK_ASSERT (rc == 0);

#ifdef HAVE_IBV_MODIFY_QP_RATE_LIMIT
    if (arg_qp_limit_burst_rate)
    {
        /* Only the mlx5 provider has ibv_modify_qp_rate_limit() */
        struct ibv_qp_rate_limit_attr rate_limit_attr =
        {
            .rate_limit = arg_qp_rate_limit_kbps,
            .max_burst_sz = sizeof (ethercat_frame_t),
            .typical_pkt_sz = sizeof (ethercat_frame_t)
        };
        rc = ibv_modify_qp_rate_limit (qp, &rate_limit_attr);
        if (rc != 0)
        {
            fprintf (stderr, "ibv_modify_qp_rate_limit() failed with rc=%d\n", rc);
            exit (EXIT_FAILURE);
        }
    }
#endif

    struct ibv_sge sg_entry;
    struct ibv_send_wr wr;
    struct ibv_send_wr *bad_wr;
    struct ibv_wc wc;
    uint32_t next_tx_sequence_number = 1;
    uint32_t destination_tested_port_index = 0;
    uint32_t source_port_offset = 1;
    uint32_t tx_buffer_index = 0;

    /* Initialise work request - flags and addr updated as packets are sent */
    sg_entry.lkey = mr->lkey;
    sg_entry.length = sizeof (ethercat_frame_t);
    sg_entry.addr = 0;

    memset (&wr, 0, sizeof (wr));
    wr.num_sge = 1;
    wr.sg_list = &sg_entry;
    wr.next = NULL;
    wr.opcode = IBV_WR_SEND;

    struct ibv_values_ex rt_values =
    {
        .comp_mask = IBV_VALUES_MASK_RAW_CLOCK
    };
    const bool tx_rate_limited = arg_max_frame_rate_hz > 0;
    rc = ibv_query_rt_values_ex (rdma_device, &rt_values);
    CHECK_ASSERT (rc == 0);
    const uint64_t hca_start_ticks = rt_values.raw_clock.tv_nsec;
    const int64_t test_start_time = get_monotonic_time ();
    const int64_t test_stop_time = test_start_time + (arg_test_interval_secs * NSECS_PER_SEC);
    const int64_t tx_interval = NSECS_PER_SEC / arg_max_frame_rate_hz;
    int64_t tx_time_of_next_frame = test_start_time;
    int64_t now;
    uint64_t total_frames_queued = 0;
    uint64_t total_frames_sent = 0;
    bool test_time_expired = false;
    bool test_complete = false;
    bool queueing_complete = false;

    /* Only poll for completion every half queue */
    uint32_t num_pending_completion = 0u;
    const uint32_t completion_ack_interval = SQ_NUM_DESC / 2;

    while (!test_complete)
    {
        /* Poll for completion. This shouldn't have to wait as only done every half queue */
        if (num_pending_completion == completion_ack_interval)
        {
            int num_completed;

            do
            {
                num_completed = ibv_poll_cq (cq, 1, &wc);

            } while (num_completed == 0);

            CHECK_ASSERT (num_completed == 1);
            CHECK_ASSERT (wc.status == IBV_WC_SUCCESS);
            total_frames_sent += num_pending_completion;
            num_pending_completion = 0;
            if (test_time_expired && (total_frames_sent == total_frames_queued))
            {
                test_complete = true;
            }
        }

        /* Determine when to send the next frame, or stop the test */
        now = get_monotonic_time ();
        bool send_next_frame = false;
        test_time_expired = now >= test_stop_time;
        if (!queueing_complete)
        {
            if (tx_rate_limited)
            {
                if (now > tx_time_of_next_frame)
                {
                    send_next_frame = true;
                    tx_time_of_next_frame += tx_interval;
                }
            }
            else
            {
                send_next_frame = true;
            }
        }

        if (send_next_frame)
        {
            /* Send the next test frame */
            sg_entry.addr = (uint64_t) &tx_frames[tx_buffer_index];
            wr.send_flags = (num_pending_completion == (completion_ack_interval - 1)) ? IBV_SEND_SIGNALED : 0;
            create_test_frame (&tx_frames[tx_buffer_index],
                    tested_port_indices[destination_tested_port_index],
                    tested_port_indices[(destination_tested_port_index + source_port_offset) % num_tested_port_indices],
                    next_tx_sequence_number);
            rc = ibv_post_send (qp, &wr, &bad_wr);
            CHECK_ASSERT (rc == 0);
            tx_buffer_index = (tx_buffer_index + 1) % SQ_NUM_DESC;
            num_pending_completion++;
            total_frames_queued++;
            if (test_time_expired && (num_pending_completion == completion_ack_interval))
            {
                queueing_complete = true;
            }

            /* Advance to the next frame which will be transmitted */
            next_tx_sequence_number++;
            destination_tested_port_index = (destination_tested_port_index + 1) % num_tested_port_indices;
            if (destination_tested_port_index == 0)
            {
                source_port_offset++;
                if (source_port_offset == num_tested_port_indices)
                {
                    source_port_offset = 1;
                }
            }
        }
    }

    /* Report the number of frames transmitted, and the average rate */
    const int64_t elapsed_duration_ns = get_monotonic_time () - test_start_time;
    rc = ibv_query_rt_values_ex (rdma_device, &rt_values);
    CHECK_ASSERT (rc == 0);
    const uint64_t hca_end_ticks = rt_values.raw_clock.tv_nsec;
    const double elapsed_duration_secs = (double) elapsed_duration_ns / 1E9;
    const double frame_rate = (double) total_frames_sent / elapsed_duration_secs;

    printf ("Send %" PRIu64 " frames over %.6f secs (CLOCK_MONOTONIC), average %.1f Hz\n",
            total_frames_sent, elapsed_duration_secs, frame_rate);

    /* Display the elapsed time as measured by the HCA core clock.
     * While struct ibv_values_ex defines the raw_clock field of type struct timespec, the mlx5_query_rt_values()
     * function in providers/mlx5/verbs.c:
     * - Sets the tv_sec field as zero
     * - Sets the tv_nsec field as the cycles output from mlx5_read_clock()
     *
     * The mlx5_read_clock() cycles output is a uint64_t populated from two uint32_t lo and hi reads from
     * ctx->hca_core_clock. ctx->hca_core_clock is mapped to a page in /dev/infiniband/uverbs0
     *
     * Presumably the 64-bit cycles count wrap according to completion_timestamp_mask in struct ibv_device_attr_ex.
     *
     * Only the mlx4 and mlx5 providers have ibv_query_rt_values_ex()
     */
    const double hca_core_clock_hz = ((double) device_attributes.hca_core_clock) * 1E3;
    const uint64_t elapsed_hca_ticks = (hca_end_ticks - hca_start_ticks) & device_attributes.completion_timestamp_mask;
    printf ("HCA elapsed time = %.6f secs\n", ((double) elapsed_hca_ticks) / hca_core_clock_hz);

    rc = ibv_destroy_qp (qp);
    CHECK_ASSERT (rc == 0);

    rc = ibv_dereg_mr (mr);
    CHECK_ASSERT (rc == 0);

    rc = ibv_destroy_cq (cq);
    CHECK_ASSERT (rc == 0);

    rc = ibv_dealloc_pd (pd);
    CHECK_ASSERT (rc == 0);

    return EXIT_SUCCESS;
}
