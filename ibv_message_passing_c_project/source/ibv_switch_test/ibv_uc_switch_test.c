/*
 * @file ibv_uc_switch_test.c
 * @date 9 Jan 2022
 * @author Chester Gillon
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <inttypes.h>

#include <limits.h>
#include <unistd.h>

#include <infiniband/verbs.h>


/* Used to index arrays indexed by the host interfaces "ends" used to transmit/receive frames, where which end is used
 * for transmit/receive is alternated to exercise the MAC addresses allocated to each end.
 *
 * The switch under test will see two MACs between each port under test, one MAC per end.
 */
typedef enum
{
    HOST_PORT_END_A,
    HOST_PORT_END_B,
    HOST_PORT_END_ARRAY_SIZE
} host_port_end_t;


/** Defines the unique identity used for one switch port under test.
 *  The VLAN is used by the injection switch.
 *
 *  The switch_port_number is that of the switch under test, controlled by the cabling and the VLAN assignment,
 *  i.e. for information and not set by the software.
 */
typedef struct
{
    uint32_t switch_port_number;
    uint16_t vlan;
} port_id_t;


/** Define a VLAN for each possible switch port under test. */
#define NUM_DEFINED_PORTS 48
static const port_id_t test_ports[NUM_DEFINED_PORTS] =
{
    { .switch_port_number =  1, .vlan = 1001},
    { .switch_port_number =  2, .vlan = 1002},
    { .switch_port_number =  3, .vlan = 1003},
    { .switch_port_number =  4, .vlan = 1004},
    { .switch_port_number =  5, .vlan = 1005},
    { .switch_port_number =  6, .vlan = 1006},
    { .switch_port_number =  7, .vlan = 1007},
    { .switch_port_number =  8, .vlan = 1008},
    { .switch_port_number =  9, .vlan = 1009},
    { .switch_port_number = 10, .vlan = 1010},
    { .switch_port_number = 11, .vlan = 1011},
    { .switch_port_number = 12, .vlan = 1012},
    { .switch_port_number = 13, .vlan = 1013},
    { .switch_port_number = 14, .vlan = 1014},
    { .switch_port_number = 15, .vlan = 1015},
    { .switch_port_number = 16, .vlan = 1016},
    { .switch_port_number = 17, .vlan = 1017},
    { .switch_port_number = 18, .vlan = 1018},
    { .switch_port_number = 19, .vlan = 1019},
    { .switch_port_number = 20, .vlan = 1020},
    { .switch_port_number = 21, .vlan = 1021},
    { .switch_port_number = 22, .vlan = 1022},
    { .switch_port_number = 23, .vlan = 1023},
    { .switch_port_number = 24, .vlan = 1024},
    { .switch_port_number = 25, .vlan = 1025},
    { .switch_port_number = 26, .vlan = 1026},
    { .switch_port_number = 27, .vlan = 1027},
    { .switch_port_number = 28, .vlan = 1028},
    { .switch_port_number = 29, .vlan = 1029},
    { .switch_port_number = 30, .vlan = 1030},
    { .switch_port_number = 31, .vlan = 1031},
    { .switch_port_number = 32, .vlan = 1032},
    { .switch_port_number = 33, .vlan = 1033},
    { .switch_port_number = 34, .vlan = 1034},
    { .switch_port_number = 35, .vlan = 1035},
    { .switch_port_number = 36, .vlan = 1036},
    { .switch_port_number = 37, .vlan = 1037},
    { .switch_port_number = 38, .vlan = 1038},
    { .switch_port_number = 39, .vlan = 1039},
    { .switch_port_number = 40, .vlan = 1040},
    { .switch_port_number = 41, .vlan = 1041},
    { .switch_port_number = 42, .vlan = 1042},
    { .switch_port_number = 43, .vlan = 1043},
    { .switch_port_number = 44, .vlan = 1044},
    { .switch_port_number = 45, .vlan = 1045},
    { .switch_port_number = 46, .vlan = 1046},
    { .switch_port_number = 47, .vlan = 1047},
    { .switch_port_number = 48, .vlan = 1048},
};


/* Array which defines which of the indices in test_ports[] are tested at run-time.
 * This allows command line arguments to specify a sub-set of the possible ports to be tested. */
static uint32_t tested_port_indices[NUM_DEFINED_PORTS];
static uint32_t num_tested_port_indices;


/* Defines the message sent over a Unreliable Connected Queue-Pair using RoCEv1.
 * This is so that Ethernet frames 1518 bytes long, the maximum non-jumbo size including a VLAN, are sent where the
 * size breakdown is:
 * - 14 bytes of Ethernet header - source MAC, destination MAC, Ethertype 802.1Q Virtual LAN (0x8100)
 * - 4 bytes for VLAN tag - the type is RDMA over Converged Ethernet (0x8915)
 * - 40 bytes for Infiniband Global Route Header
 * - 12 bytes for Base Transport Header
 * - 1444 bytes (REQUIRED_UC_MESSAGE_SIZE_BYTES) for the UC message size
 * - 4 bytes for Invariant CRC
 */
#define REQUIRED_UC_MESSAGE_SIZE_BYTES 1444
#define UC_MESSAGE_NUM_HEADER_WORDS    5 /* test_sequence_number .. destination_qp_num */
typedef struct
{
    /* The sequence number incremented for every message sent, used to identify expected received messages */
    uint32_t test_sequence_number;
    /* Indices into test_ports[] for the source and destination switch ports used for the message.
     * Can be used upon receipt to identify the ports used without having to search other fields. */
    uint32_t source_port_index;
    uint32_t destination_port_index;
    /* The Queue-Pair numbers used for the source and destination of the message.
     * Used to cross-check that the message was received on the expected Queue-Pair.
     * While the switch under test may flood frames with an unknown destination MAC address to multiple switch ports,
     * which get assigned to different VLANs in the injection switch, a UC Queue-Pair ignores received frames which
     * are on a different VLAN to that expected so flooded frames won't be received.
     */
    uint32_t source_qp_num;
    uint32_t destination_qp_num;
    /* Used to pad the message to get the required Ethernet frame size. Contents is not used. */
    uint8_t data[REQUIRED_UC_MESSAGE_SIZE_BYTES - (UC_MESSAGE_NUM_HEADER_WORDS * sizeof (uint32_t))];
} uc_test_message_t;


/* Identifies one type of frame recorded by the test, as a UC message */
typedef enum
{
    /* A frame transmitted by the test */
    FRAME_RECORD_TX_TEST_FRAME,
    /* A test frame in the format which is transmitted, and which was received by the test on the
     * Queue-Pair the expected destination port. This frame contains an expected pending sequence number. */
    FRAME_RECORD_RX_TEST_FRAME,
    /* As per FRAME_RECORD_RX_TEST_FRAME except the received sequence number was not expected.
     * This may happen if frames get delayed such that there is no record of the pending sequence number. */
    FRAME_RECORD_RX_UNEXPECTED_FRAME,
    /* A frame received by the test, which isn't one transmitted.
       I.e. any frames which are not generated by the test program. */
    FRAME_RECORD_RX_OTHER,

    FRAME_RECORD_ARRAY_SIZE
} frame_record_type_t;


/* Look up table which gives the description of each frame_record_type_t */
static const char *const frame_record_types[FRAME_RECORD_ARRAY_SIZE] =
{
    [FRAME_RECORD_TX_TEST_FRAME      ] = "Tx Test",
    [FRAME_RECORD_RX_TEST_FRAME      ] = "Rx Test",
    [FRAME_RECORD_RX_UNEXPECTED_FRAME] = "Rx Unexpected",
    [FRAME_RECORD_RX_OTHER           ] = "Rx Other"
};


/* Used to record one frame transmitted or received during the test, for debugging purposes */
typedef struct
{
    /* Identifies the type of frame */
    frame_record_type_t frame_type;
    /* The relative time from the start of the test that the frame was sent or received.
       This is using the monotonic time used by the test busy-polling loop. */
    int64_t relative_test_time;
    /* The length of the received UC message payload */
    uint32_t byte_len;
    /* When frame_type is other than FRAME_RECORD_RX_OTHER the sequence number of the frame.
     * Allows received frames to be matched against the transmitted frame. */
    uint32_t test_sequence_number;
    /* When frame_type is other than FRAME_RECORD_RX_OTHER the source and destination port numbers of the frame,
     * based upon the payload content which is cross-checked against the local qp_num in the receive completion. */
    uint32_t source_port_index;
    uint32_t destination_port_index;
    /* Set true for a FRAME_RECORD_TX_TEST_FRAME for which there is no matching FRAME_RECORD_RX_TEST_FRAME */
    bool frame_missed;
} frame_record_t;


/* Command line argument which specifies the test interval in seconds, which is the interval over which statistics are
 * accumulated and then reported. */
static int64_t arg_test_interval_secs = 10;


/* Command line argument which specifies the maximum rate at which transmit frames can be sent.
 * If <= 0 then no maximum rate is applied, i.e. transmits as quickly as possible. */
static int64_t arg_max_frame_rate_hz = -1;
static bool arg_max_frame_rate_hz_set = false;

/* Command line arguments which specify the RDMA device names and ports used for each end used to transmit/receive test
 * frames on the host. */
static char arg_rdma_device_names[HOST_PORT_END_ARRAY_SIZE][PATH_MAX];
static uint8_t arg_rdma_ports[HOST_PORT_END_ARRAY_SIZE];


/* Command line argument which controls how the test runs:
 * - When false the test runs until requested to stop, and only reports summary information for each test interval.
 * - When true the test runs for a single test interval, recording the transmitted/received frames in memory which are written
 *   to s CSV file at the end of the test interval. */
static bool arg_frame_debug_enabled = false;


/* The number of frames which can be posted for transmit. Only one is signalled for completion to limit the overhead
 * of polling for completion. Set to a low value since the software can limit the transmit rate to avoid overloading the
 * switch under test, and don't want a large burst of frames transmitted by the RDMA hardware. */
#define NUM_PENDING_TX_FRAMES 10


/* Used to store pending receive frames for one source / destination port combination.
 * As frames are transmitted they are stored in, and then removed once received.
 *
 * Pending receive frames are stored for each source / destination port combination since:
 * a. Frames for a given source / destination port combination shouldn't get reordered.
 * b. Upon receipt saves having to search through all pending frames.
 *
 * NOMINAL_TOTAL_PENDING_RX_FRAMES defines the total number of pending receive frames which can be stored across all
 * source / destination port combinations, and allows for both latency in the test frames being circulated around the switches
 * and any delay in the software polling for frame receipt. It's value is set based upon the initial code which fixed to test
 * 24 switch ports (23x24 so 552 combinations) and 3 pending rx frames per combination, and doubled to give some leeway.
 *
 * The actual number of pending receive frames per combination is set at run time according to the number of ports being tested.
 *
 * Missing frames get detected either:
 * a. If a single frame is missing, the missing frame is detected when the next expected frame has a later sequence number.
 * b. If multiple frames are missing, a missing frame is detected when the next transmit occurs and the
 *    pending_rx_sequence_numbers[] array is full. */
#define NOMINAL_TOTAL_PENDING_RX_FRAMES 3312
#define MIN_TESTED_PORTS 2
#define MIN_PENDING_RX_FRAMES_PER_PORT_COMBO 3
typedef struct
{
    /* The number of frames which have been transmitted and are pending for receipt */
    uint32_t num_pending_rx_frames;
    /* Index in pending_rx_sequence_numbers[] where a frame is stored after has been transmitted */
    uint32_t tx_index;
    /* Index in pending_rx_sequence_numbers[] which contains the next expected receive frame */
    uint32_t rx_index;
    /* Circular buffer used to record pending sequence numbers for receive frames */
    uint32_t *pending_rx_sequence_numbers;
    /* When frame debug is enabled points at the frame record for the FRAME_RECORD_TX_TEST_FRAME for each pending receive frame,
     * so that if a frame is not received the transmit frame record can be marked as such.
     *
     * Done this way round to avoid marking any transmits frames at the end of a test sequence are not marked as missing. */
    frame_record_t **tx_frame_records;
} pending_rx_frames_t;


/* Used to form the memory region for one RDMA host port to transmit or receive messages.
 * Haven't attempted to align the messages. */
typedef struct
{
    uc_test_message_t tx_messages[NUM_PENDING_TX_FRAMES];
    uc_test_message_t rx_messages[NOMINAL_TOTAL_PENDING_RX_FRAMES];
} host_port_uc_messages;


/* Defines the RDMA context used for one host endpoint used to transmit/receive test frames.
 * To allow for operation with RDMA devices which only have one port, each endpoint has its own protection domain,
 * and thus memory region and SRQ. */
typedef struct
{
    /* The port number used on the RDMA device */
    uint8_t rdma_port_num;
    /* The RDMA device */
    struct ibv_context *rdma_device;
    struct ibv_device_attr device_attributes;
    /* The attributes of the port used on the RDMA device */
    struct ibv_port_attr port_attributes;
    /* The GIDs and their indices for all defined switch ports.
     * All entries are populated regardless of the number of ports being tested, since Queue-Pairs are initialised for all
     * defined switch ports as a way of checking for any overheads of the maximum number of Queue-Pairs on the achieved
     * maximum frame rate. */
    uint8_t gid_indices[NUM_DEFINED_PORTS];
    union ibv_gid gids[NUM_DEFINED_PORTS];
    /* Protection domain for the RDMA device */
    struct ibv_pd *pd;
    /* The completion queue used for transmit/receive messages */
    struct ibv_cq *cq;
    /* The memory region which provides access to test_messages */
    struct ibv_mr *mr;
    /* The shared receive queue used to post work-requests for any of the Queue-Pairs for this endpoint */
    struct ibv_srq *srq;
    /* Queue-Pairs, indexed by each [source_port][destination_port] combination.
     * The elements for source_port==destination are not used. */
    struct ibv_qp *qps[NUM_DEFINED_PORTS][NUM_DEFINED_PORTS];
    /* Allocates space for the transmit/receive messages, for which work requests can be posted */
    host_port_uc_messages test_messages;
} host_port_endpoint_t;


/* The context used for the thread which sends/receive the test frames */
typedef struct
{
    /* The pair of RDMA ports on the host used to transmit/receive test frames */
    host_port_endpoint_t host_ports[HOST_PORT_END_ARRAY_SIZE];
} frame_tx_rx_thread_context_t;


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
    printf ("Usage %s: -i <end_a_device>,<end_a_port>,<end_b_device,end_b_port> [-t <duration_secs>] [-d] [-p <port_list>] [-r <rate_hz>]\n", program_name);
    printf ("\n");
    printf ("  -i specifies the RDMA interfaces used to send/receive frames.\n");
    printf ("     The use of RDMA Queue-Pairs requires different RDMA interfaces to be used\n");
    printf ("     for send and receive to ensure the frames are output on Ethernet rather\n");
    printf ("     looped-back internally on the HCA. This is a comma separated list of the\n");
    printf ("     end A RDMA device, end A RDMA port, end B RDMA device and end B RDMA port.\n");
    printf ("     Where messages are sent from both end A -> end B and the reverse direction\n");
    printf ("     so that the switch under test can learn all MAC addresses used; end A and B\n");
    printf ("     have the same VLANs but different MAC addresses allocated.\n");
    printf ("  -d enables debug mode, where runs just for a single test interval and creates\n");
    printf ("     a CSV file containing the frames sent/received.\n");
    printf ("  -t defines the duration of a test interval in seconds, over which the number\n");
    printf ("     errors is accumulated and reported.\n");
    printf ("  -p defines which switch ports to test. The <port_list> is a comma separated\n");
    printf ("     string, with each item either a single port number or a start and end port\n");
    printf ("     range delimited by a dash. E.g. 1,4-6 specifies ports 1,4,5,6.\n");
    printf ("     If not specified defaults to all %u defined ports\n", NUM_DEFINED_PORTS);
    printf ("  -r Specifies the maximum rate at which frames are transmitted.\n");
    printf ("     Using a value <= 0 means the frame transmission rate is not limited,\n");
    printf ("     even when testing a small number of ports.\n");

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
    const char *const optstring = "i:dt:p:r:";
    bool rdma_interfaces_specified = false;
    int option;
    char junk;

    /* Process the command line arguments */
    option = getopt (argc, argv, optstring);
    while (option != -1)
    {
        switch (option)
        {
        case 'i':
            {
                const char *const delim = ",";
                char *const interface_list = strdup (optarg);
                char *saveptr;
                char *token = strtok_r (interface_list, delim, &saveptr);

                for (host_port_end_t port_end = 0; port_end < HOST_PORT_END_ARRAY_SIZE; port_end++)
                {
                    if (token != NULL)
                    {
                        snprintf (arg_rdma_device_names[port_end], sizeof (arg_rdma_device_names[port_end]), "%s", token);
                    }
                    else
                    {
                        printf ("Error: device not specified in interface list %s\n", optarg);
                        exit (EXIT_FAILURE);
                    }
                    token = strtok_r (NULL, delim, &saveptr);

                    if (token != NULL)
                    {
                        uint32_t port;

                        if ((sscanf (token, "%" SCNu32 "%c", &port, &junk) != 1) || (port == 0) || (port > 255))
                        {
                            printf ("Error: Invalid port %s in interface list %s\n", token, optarg);
                            exit (EXIT_FAILURE);
                        }
                        arg_rdma_ports[port_end] = (uint8_t) port;
                    }
                    else
                    {
                        printf ("Error: port not specified in interface list %s\n", optarg);
                        exit (EXIT_FAILURE);
                    }
                    token = strtok_r (NULL, delim, &saveptr);
                }
                if (token != NULL)
                {
                    printf ("Error: Excess values in interface list %s\n", optarg);
                    exit (EXIT_FAILURE);
                }
                rdma_interfaces_specified = true;
            }
            break;

        case 'd':
            arg_frame_debug_enabled = true;
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

        case '?':
        default:
            display_usage (program_name);
            break;
        }

        option = getopt (argc, argv, optstring);
    }

    /* Check the expected arguments have been provided */
    if (!rdma_interfaces_specified)
    {
        printf ("Error: The RDMA interfaces must be specified\n\n");
        display_usage (program_name);
    }

    if (num_tested_port_indices < MIN_TESTED_PORTS)
    {
        printf ("Error: A minimum of %d ports must be tested\n\n", MIN_TESTED_PORTS);
        display_usage (program_name);
    }

    if (optind < argc)
    {
        printf ("Error: Unexpected nonoption (first %s)\n\n", argv[optind]);
        display_usage (program_name);
    }
}


/**
 * @brief Find the GID on a host RDMA endpoint to be used to send/receive frames on the VLAN for switch port under test
 * @details This searches the sysfs files for the RDMA endpoint to find the GID
 * @param[in/out] endpoint The endpoint to find the GID for
 * @param[in] tested_port_index Which switch port under test to find the GID for
 */
static void find_gid_for_endpoint_vlan (host_port_endpoint_t *const endpoint, const uint32_t tested_port_index)
{
    char sysfs_path[PATH_MAX];
    bool found_gid;
    FILE *sysfs_file;
    char gid_type[80];
    char device_name[80];
    const char *const used_gid_type = "IB/RoCE V1";
    const char *vlan_suffix_start;
    uint32_t device_vlan;
    int rc;
    const port_id_t *const tested_port = &test_ports[tested_port_index];

    /* Search all possible GID table entries for the port VLAN */
    found_gid = false;
    for (int gid_index = 0; (!found_gid) && (gid_index < endpoint->port_attributes.gid_tbl_len); gid_index++)
    {
        /* First check for a RoCEv1 GID, since the message size used by this program to get the expected Ethernet frame size
         * assumes RoCEv1 framing. */
        snprintf (sysfs_path, sizeof (sysfs_path), "%s/ports/%" PRIu8 "/gid_attrs/types/%d",
                endpoint->rdma_device->device->ibdev_path, endpoint->rdma_port_num, gid_index);
        sysfs_file = fopen (sysfs_path, "r");
        if (sysfs_file == NULL)
        {
            continue;
        }
        if (fgets (gid_type, sizeof (gid_type), sysfs_file) == NULL)
        {
            continue;
        }
        fclose (sysfs_file);
        if (strncasecmp (gid_type, used_gid_type, strlen (used_gid_type)) != 0)
        {
            continue;
        }

        /* Get the VLAN for the GID by extracting the numeric suffix on the Ethernet device name */
        snprintf (sysfs_path, sizeof (sysfs_path), "%s/ports/%" PRIu8 "/gid_attrs/ndevs/%" PRIu8,
                endpoint->rdma_device->device->ibdev_path, endpoint->rdma_port_num, gid_index);
        sysfs_file = fopen (sysfs_path, "r");
        if (sysfs_file == NULL)
        {
            continue;
        }
        if (fgets (device_name, sizeof (device_name), sysfs_file) == NULL)
        {
            continue;
        }
        fclose (sysfs_file);
        vlan_suffix_start = strrchr (device_name, '.');
        if (vlan_suffix_start == NULL)
        {
            continue;
        }
        if (sscanf (vlan_suffix_start, ".%" SCNu32, &device_vlan) != 1)
        {
            continue;
        }

        /* Check if the device VLAN is the requested one */
        found_gid = device_vlan == tested_port->vlan;
        if (found_gid)
        {
            endpoint->gid_indices[tested_port_index] = (uint8_t) gid_index;
            rc = ibv_query_gid (endpoint->rdma_device, endpoint->rdma_port_num, gid_index, &endpoint->gids[tested_port_index]);
            CHECK_ASSERT (rc == 0);
        }
    }

    if (!found_gid)
    {
        printf ("Error: For %s port %" PRIu8 " unable to find RoCEv1 GID for VLAN %" PRIu16 "\n",
                endpoint->rdma_device->device->name, endpoint->rdma_port_num, tested_port->vlan);
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Obtain a pseudo-random 24-bit Packet Sequence Number
 * @return Returns the starting Packet Sequence Number
 */
static uint32_t get_random_psn (void)
{
    return lrand48 () & 0xffffff;
}


/**
 * @brief Open the host RDMA endpoints, using the RDMA devices and ports specified in the command line arguments.
 * @param[in/out] tx_rx_thread_context The thread context to store the endpoints in.
 */
static void open_host_endpoints (frame_tx_rx_thread_context_t *const tx_rx_thread_context)
{
    int num_ibv_devices = 0;
    struct ibv_device **device_list;
    int rc;
    uint32_t tested_port_index;
    uint32_t source_port_index;
    uint32_t destination_port_index;
    struct ibv_qp_attr qp_attr;

    /* Find all RDMA devices */
    device_list = ibv_get_device_list (&num_ibv_devices);
    check_assert (num_ibv_devices > 0, "No RDMA devices found");

    /* Perform the initialisation which can operate on each endpoint independently */
    for (host_port_end_t port_end = 0; port_end < HOST_PORT_END_ARRAY_SIZE; port_end++)
    {
        host_port_endpoint_t *const endpoint = &tx_rx_thread_context->host_ports[port_end];

        /* Open the RDMA device */
        endpoint->rdma_device = NULL;
        for (int device_index = 0; (endpoint->rdma_device == NULL) && (device_index < num_ibv_devices); device_index++)
        {
            if (strcmp (device_list[device_index]->name, arg_rdma_device_names[port_end]) == 0)
            {
                endpoint->rdma_device = ibv_open_device (device_list[device_index]);
                CHECK_ASSERT (endpoint->rdma_device != NULL);
            }
        }

        if (endpoint->rdma_device == NULL)
        {
            printf ("Error: Unable to find RDMA device %s\n", arg_rdma_device_names[port_end]);
            exit (EXIT_FAILURE);
        }

        rc = ibv_query_device (endpoint->rdma_device, &endpoint->device_attributes);
        CHECK_ASSERT (rc == 0);

        /* Get the RDMA port attributes */
        endpoint->rdma_port_num = arg_rdma_ports[port_end];
        if (endpoint->rdma_port_num > endpoint->device_attributes.phys_port_cnt)
        {
            printf ("Error: Port %" PRIu8 " doesn't exist on %s\n", endpoint->rdma_port_num, arg_rdma_device_names[port_end]);
            exit (EXIT_FAILURE);
        }
        rc = ibv_query_port (endpoint->rdma_device, endpoint->rdma_port_num, &endpoint->port_attributes);
        CHECK_ASSERT (rc == 0);

        /* Find the GIDs to be used for the tested VLANs on the RDMA port */
        for (tested_port_index = 0; tested_port_index < NUM_DEFINED_PORTS; tested_port_index++)
        {
            find_gid_for_endpoint_vlan (endpoint, tested_port_index);
        }

        /* Allocate protection domain */
        endpoint->pd = ibv_alloc_pd (endpoint->rdma_device);
        CHECK_ASSERT (endpoint->pd != NULL);

        /* Allocation completion queue, sized for transmit and receive.
         * While only transmit message is signalled for completion, allows space in case the transmit completes with an error. */
        endpoint->cq = ibv_create_cq (endpoint->rdma_device, NUM_PENDING_TX_FRAMES + NOMINAL_TOTAL_PENDING_RX_FRAMES,
                NULL, NULL, 0);
        CHECK_ASSERT (endpoint->cq != NULL);

        /* Register memory region to access the test_messages */
        endpoint->mr = ibv_reg_mr (endpoint->pd, &endpoint->test_messages, sizeof (endpoint->test_messages), IBV_ACCESS_LOCAL_WRITE);
        CHECK_ASSERT (endpoint->mr != NULL);

        /* Create the shared receive queue to be able to post a work-request with a single SGE for receive receive message buffer */
        struct ibv_srq_init_attr srq_init_attr =
        {
            .attr.max_wr = NOMINAL_TOTAL_PENDING_RX_FRAMES,
            .attr.max_sge = 1
        };
        endpoint->srq = ibv_create_srq (endpoint->pd, &srq_init_attr);
        CHECK_ASSERT (endpoint->srq != NULL);

        for (source_port_index = 0; source_port_index < NUM_DEFINED_PORTS; source_port_index++)
        {
            for (destination_port_index = 0; destination_port_index < NUM_DEFINED_PORTS; destination_port_index++)
            {
                if (source_port_index != destination_port_index)
                {
                    /* Create a Queue-Pair for each defined combination of source/destination switch ports.
                     * As a Shared Receive Queue is used only has to specify the send capability. */
                    struct ibv_qp_init_attr qp_init_attr =
                    {
                        .send_cq = endpoint->cq,
                        .recv_cq = endpoint->cq,
                        .srq = endpoint->srq,
                        .cap =
                        {
                            .max_send_wr = NUM_PENDING_TX_FRAMES,
                            .max_send_sge = 1
                        },
                        .qp_type = IBV_QPT_UC,
                        .sq_sig_all = 0 /* Since only one of out every NUM_PENDING_TX_FRAMES is signalled for completion */
                    };

                    endpoint->qps[source_port_index][destination_port_index] = ibv_create_qp (endpoint->pd, &qp_init_attr);
                    CHECK_ASSERT (endpoint->qps[source_port_index][destination_port_index] != NULL);

                    /* Transition the Queue-Pair to Init state */
                    memset (&qp_attr, 0, sizeof (qp_attr));
                    qp_attr.qp_state = IBV_QPS_INIT;
                    qp_attr.pkey_index = 0;
                    qp_attr.port_num = endpoint->rdma_port_num;
                    qp_attr.qp_access_flags = IBV_ACCESS_LOCAL_WRITE;
                    rc = ibv_modify_qp (endpoint->qps[source_port_index][destination_port_index], &qp_attr,
                            IBV_QP_STATE | IBV_QP_PKEY_INDEX | IBV_QP_PORT | IBV_QP_ACCESS_FLAGS);
                    CHECK_ASSERT (rc == 0);
                }
                else
                {
                    endpoint->qps[source_port_index][destination_port_index] = NULL;
                }
            }
        }
    }

    ibv_free_device_list (device_list);

    /* Complete the Queue-Pair initialisation which requires information about both the Queue-Pairs on both host RDMA ports */
    for (source_port_index = 0; source_port_index < NUM_DEFINED_PORTS; source_port_index++)
    {
        for (destination_port_index = 0; destination_port_index < NUM_DEFINED_PORTS; destination_port_index++)
        {
            if (source_port_index != destination_port_index)
            {
                for (host_port_end_t send_end = 0; send_end < HOST_PORT_END_ARRAY_SIZE; send_end++)
                {
                    const host_port_end_t recv_end = (send_end == HOST_PORT_END_A) ? HOST_PORT_END_B : HOST_PORT_END_A;
                    host_port_endpoint_t *const send_endpoint = &tx_rx_thread_context->host_ports[send_end];
                    host_port_endpoint_t *const recv_endpoint = &tx_rx_thread_context->host_ports[recv_end];
                    const uint32_t psn = get_random_psn ();

                    /* Transition the Queue-Pair to Ready To Receive.
                     * The path_mtu is set to the next largest size to ensure that uc_test_message_t results in a single
                     * transmitted RoCEv1 Ethernet frame. */
                    memset (&qp_attr, 0, sizeof (qp_attr));
                    qp_attr.qp_state = IBV_QPS_RTR;
                    qp_attr.ah_attr.is_global = true;
                    qp_attr.ah_attr.grh.hop_limit = 1;
                    qp_attr.ah_attr.grh.dgid = recv_endpoint->gids[destination_port_index];
                    qp_attr.ah_attr.grh.sgid_index = send_endpoint->gid_indices[source_port_index];
                    qp_attr.ah_attr.port_num = send_endpoint->rdma_port_num;
                    qp_attr.path_mtu = IBV_MTU_2048;
                    qp_attr.dest_qp_num = recv_endpoint->qps[source_port_index][destination_port_index]->qp_num;
                    qp_attr.rq_psn = psn;
                    rc = ibv_modify_qp (send_endpoint->qps[source_port_index][destination_port_index], &qp_attr,
                            IBV_QP_STATE | IBV_QP_AV | IBV_QP_PATH_MTU | IBV_QP_DEST_QPN | IBV_QP_RQ_PSN);
                    CHECK_ASSERT (rc == 0);

                    /* Transition the Queue-Pair to Ready To Send */
                    memset (&qp_attr, 0, sizeof (qp_attr));
                    qp_attr.qp_state = IBV_QPS_RTS;
                    qp_attr.sq_psn = psn;
                    rc = ibv_modify_qp (send_endpoint->qps[source_port_index][destination_port_index], &qp_attr,
                            IBV_QP_STATE |  IBV_QP_SQ_PSN);
                    CHECK_ASSERT (rc == 0);
                }
            }
        }
    }
}


int main (int argc, char *argv[])
{
    int rc;

    read_command_line_arguments (argc, argv);

    /* Add protection against fork() being called */
    rc = ibv_fork_init ();
    CHECK_ASSERT (rc == 0);

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    frame_tx_rx_thread_context_t *const tx_rx_thread_context = calloc (1, sizeof (*tx_rx_thread_context));
    open_host_endpoints (tx_rx_thread_context);

    return EXIT_SUCCESS;
}
