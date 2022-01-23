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

#include <sys/time.h>
#include <limits.h>
#include <unistd.h>
#include <semaphore.h>
#include <signal.h>

#include <infiniband/verbs.h>


/* Define a string to report the Operating System just to report in result filenames,
 * when comparing results from multiple runs in the same directory in a PC which can be dual-booted. */
#ifdef _WIN32
#define OS_NAME "windows"
#else
#define OS_NAME "linux"
#endif


#define NSECS_PER_SEC 1000000000LL


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


/* Look up table which gives the description of each host_port_end_t */
static const char *const host_port_ends[HOST_PORT_END_ARRAY_SIZE] =
{
    [HOST_PORT_END_A] = "A",
    [HOST_PORT_END_B] = "B"
};


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
    /* Which host RDMA endpoint the frame was transmitted from */
    host_port_end_t tx_end;
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


/* File used to store a copy of the output written to the console */
static FILE *console_file;


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


/* The number of frames which can be posted for transmit. Set to a low value since the software can limit the transmit rate to
 * avoid overloading the switch under test, and don't want a large burst of frames transmitted by the RDMA hardware. */
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


/* Work request identities used to determine if a work completion was for a transmit or receive message */
#define RX_WR_ID 0
#define TX_WR_ID 1


/* The length of the completion queue, for the maximum number of outstanding work-requests */
#define CQ_LENGTH (NUM_PENDING_TX_FRAMES + NOMINAL_TOTAL_PENDING_RX_FRAMES)


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
    /* Contains space to poll for completion status */
    struct ibv_wc wcs[CQ_LENGTH];
    /* Index into test_messages.tx_messages for the next message to transmitted */
    uint32_t next_tx_index;
    /* The number of free transmit buffers which are available for transmission. Decremented when a work-request is posted
     * and incremented when completion is seen. Since transmission sequences through different Queue-Pairs (with one QP
     * for each combination of source/destination port) each transmit work-request has to be signalled to track completion. */
    uint32_t num_free_tx_buffers;
    /* Index into next_rx_index.rx_messages for the next message buffer receive work-request to be posted.
     * At initialisation receive work-requests are posted for all message buffers, and as each buffer
     * work-request is polled as completed, the work-request for the buffer is re-posted to try and keep
     * all buffers posted for receive to avoid dropping messages. */
    uint32_t next_rx_index;
    /* Contains the pending receive frames, indexed by [source_port][destination_port].
     * This is stored per host endpoint to avoid issues with messages between the same combination of source/destination
     * on the switch under test being read out-of-order when polling for receipt completion on each host endpoint. */
    pending_rx_frames_t pending_rx_frames[NUM_DEFINED_PORTS][NUM_DEFINED_PORTS];
    /* Allocates space for the transmit/receive messages, for which work requests can be posted */
    host_port_uc_messages test_messages;
} host_port_endpoint_t;


/* Contains the statistics for test frames for one combination of source / destination ports for one test interval */
typedef struct
{
    /* The number of expected receive frames during the test interval */
    uint32_t num_valid_rx_frames;
    /* The number of missing receive frames during the test interval */
    uint32_t num_missing_rx_frames;
    /* The number of frames transmitted during the test interval */
    uint32_t num_tx_frames;
} port_frame_statistics_t;


/* Contains the statistics for test frames transmitted and received over one test interval in which the statistics
 * are accumulated. The transmit and receive counts may not match, if there are frames which are pending being received
 * at the end of the interval. */
typedef struct
{
    /* The monotonic start and end time of the test interval, to give the duration over which the statistics were accumulated */
    int64_t interval_start_time;
    int64_t interval_end_time;
    /* The counts of different types of frames during the test interval, across all ports tested */
    uint32_t frame_counts[FRAME_RECORD_ARRAY_SIZE];
    /* Receive frame counts, indexed by each [source_port][destination_port] combination */
    port_frame_statistics_t port_frame_statistics[NUM_DEFINED_PORTS][NUM_DEFINED_PORTS];
    /* Counts the total number of missing frames during the test interval */
    uint32_t total_missing_frames;
    /* The maximum value of num_pending_rx_frames which has been seen for any source / destination port combination during
     * the test. Used to collect debug information about how close to the MAX_PENDING_RX_FRAMES a test without any errors is
     * getting. This value can get to the maximum if missed frames get reported during the test when the transmission detects
     * all pending rx sequence numbers are in use.
     *
     * If Rx Unexpected frames are reported and max_pending_rx_frames is MAX_PENDING_RX_FRAMES this suggests the maximum value
     * should be increased to allow for the latency in the frames being sent by the switches and getting thtough the software. */
    uint32_t max_pending_rx_frames;
    /* Set true in the final statistics before the transmit/receive thread exits */
    bool final_statistics;
} frame_test_statistics_t;


/* Used to record frames transmitted or received for debug purposes */
typedef struct
{
    /* The allocated length of the frame_records[] array. When zero frames are not recorded */
    uint32_t allocated_length;
    /* The number of entries in the frame_records[] array which are currently populated */
    uint32_t num_frame_records;
    /* Array used to record frames */
    frame_record_t *frame_records;
} frame_records_t;


/* The options for how the host ports directions are changed during the test */
typedef enum
{
    /* The direction of the host ports reverses every frame, except once all port combinations tested */
    DIRECTION_REVERSE_EXCEPT_ALL_PORT_COMBINATIONS_TESTED,
    /* The direction of the host ports reverses every frame, except once the destination_tested_port_index resets */
    DIRECTION_REVERSE_EXCEPT_DESTINATION_TESTED_PORT_INDEX_RESETS
} host_port_direction_option_t;


/* The context used for the thread which sends/receive the test frames */
typedef struct
{
    /* The pair of RDMA ports on the host used to transmit/receive test frames */
    host_port_endpoint_t host_ports[HOST_PORT_END_ARRAY_SIZE];
    /* The next sequence number to be transmitted */
    uint32_t next_tx_sequence_number;
    /* The next index into tested_port_indices[] for the next destination port to use for a transmitted frame */
    uint32_t destination_tested_port_index;
    /* The next modulo offset from destination_tested_port_index to use as the source port for a transmitted frame */
    uint32_t source_port_offset;
    /* For the next test frame transmitted which of the host_ports[] elements are used for transmit/receive */
    host_port_end_t host_tx_port;
    host_port_end_t host_rx_port;
    /* Controls when the host_tx_port and host_rx_port to minimise the number of flooded frames by the switch under
     * test as it learns the MAC addresses. */
    host_port_direction_option_t host_port_direction_option;
    /* Used to accumulate the statistics for the current test interval */
    frame_test_statistics_t statistics;
    /* Monotonic time at which the current test interval ends, which is when the statistics are published and then reset */
    int64_t test_interval_end_time;
    /* Optionally used to record frames for debug */
    frame_records_t frame_recording;
    /* Controls the rate at which transmit frames are generated:
     * - When true a timer is used to limit the maximum rate at which frames are transmitted.
     *   This can be used when the available bandwidth on the link to the injection switch exceeds that available to distribute
     *   the total bandwidth across all ports in the switch under test.
     *
     *   E.g. if the link to the injection switch is 1G and the links to the switch under test are 100M, then if less than 10
     *   ports are tested then need to rate limit the transmission.
     *
     * - When false frames are transmitted as quickly as possible. */
    bool tx_rate_limited;
    /* When tx_rate_limited is true, the monotonic time between each frame transmitted */
    int64_t tx_interval;
    /* When tx_rate_limited is true, the monotonic time at which to transmit the next frame */
    int64_t tx_time_of_next_frame;
    /* The maximum number of pending receive frames for each source / destination port combination during the test */
    uint32_t pending_rx_sequence_numbers_length;
} frame_tx_rx_thread_context_t;


/* Contains the information for the results summary over multiple test intervals */
typedef struct
{
    /* Filename used for the per-port counts */
    char per_port_counts_csv_filename[PATH_MAX];
    /* File to which the per-port counts are written */
    FILE *per_port_counts_csv_file;
    /* The number of test intervals which have had failures, due to missed frames */
    uint32_t num_test_intervals_with_failures;
    /* The string containing the time of the last test interval which had a failure */
    char time_of_last_failure[80];
} results_summary_t;


/* test_statistics contains the statistics from the most recent completed test interval.
 * It is written by the transmit_receive_thread, and read by the main thread to report the test progress.
 *
 * The semphores control the access by:
 * a. The free semaphore is initialised to 1, and the populated semaphore to 0.
 * b. The main thread blocks in sem_wait (test_statistics_populated) waiting for results.
 * c. At the end of a test interval the transmit_receive_thread:
 *    - sem_wait (test_statistics_free) which should not block unless the main thread isn't keeping up with reporting
 *      the test progress.
 *    - Stores the results for the completed test interval in test_statistics
 *    - sem_post (test_statistics_populated) to wake up the main thread.
 * d. When the main thread is woken up from sem_wait(test_statistics_populated):
 *    - Reports the contents of test_statistics
 *    - sem_post (test_statistics_free) to indicate has processed test_statistics
 * e. The sequence starts again from b.
 */
static frame_test_statistics_t test_statistics;
static sem_t test_statistics_free;
static sem_t test_statistics_populated;


/* Set true in a signal handler when Ctrl-C is used to request a running test stops */
static volatile bool test_stop_requested;


/**
 * @brief Signal handler to request a running test stops
 * @param[in] sig Not used
 */
static void stop_test_handler (const int sig)
{
    test_stop_requested = true;
}


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
        printf ("Assertion failed : ");
        vprintf (format, args);
        va_end (args);
        printf ("\n");

        if (console_file != NULL)
        {
            va_start (args, format);
            fprintf (console_file, "Assertion failed : ");
            vfprintf (console_file, format, args);
            va_end (args);
            fprintf (console_file, "\n");
        }

        exit (EXIT_FAILURE);
    }
}


/*
 * @brief Write formatted output to the console and a log file
 * @param[in] format printf style format string
 * @param[in] ... printf arguments
 */
static void console_printf (const char *const format, ...) __attribute__((format(printf,1,2)));
static void console_printf (const char *const format, ...)
{
    va_list args;

    va_start (args, format);
    vfprintf (console_file, format, args);
    va_end (args);

    va_start (args, format);
    vprintf (format, args);
    va_end (args);
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
        console_printf ("clock_getime(CLOCK_MONOTONIC) failed\n");
        exit (EXIT_FAILURE);
    }

    return (now.tv_sec * NSECS_PER_SEC) + now.tv_nsec;
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
 * @brief Post the next receive buffer work-request to the shared receive queue for a host RDMA endpoint
 * @param[in,out] endpoint Which endpoint to post the receive work-request on
 */
static void post_endpoint_next_receive_wr (host_port_endpoint_t *const endpoint)
{
    struct ibv_sge sg_entry =
    {
        .addr = (uint64_t) &endpoint->test_messages.rx_messages[endpoint->next_rx_index],
        .length = sizeof (endpoint->test_messages.rx_messages[endpoint->next_rx_index]),
        .lkey = endpoint->mr->lkey
    };
    struct ibv_recv_wr recv_wr =
    {
        .wr_id = RX_WR_ID,
        .next = NULL,
        .sg_list = &sg_entry,
        .num_sge = 1
    };
    struct ibv_recv_wr *bad_recv_wr;
    int rc;

    rc = ibv_post_srq_recv (endpoint->srq, &recv_wr, &bad_recv_wr);
    CHECK_ASSERT (rc == 0);
    endpoint->next_rx_index = (endpoint->next_rx_index + 1) % NOMINAL_TOTAL_PENDING_RX_FRAMES;
}


/**
 * @brief Open the host RDMA endpoints, using the RDMA devices and ports specified in the command line arguments.
 * @param[in/out] context The thread context to store the endpoints in.
 */
static void open_host_endpoints (frame_tx_rx_thread_context_t *const context)
{
    int num_ibv_devices = 0;
    struct ibv_device **device_list;
    int rc;
    uint32_t tested_port_index;
    uint32_t source_port_index;
    uint32_t destination_port_index;
    struct ibv_qp_attr qp_attr;
    host_port_end_t port_end;

    /* Find all RDMA devices */
    device_list = ibv_get_device_list (&num_ibv_devices);
    check_assert (num_ibv_devices > 0, "No RDMA devices found");

    /* Perform the initialisation which can operate on each endpoint independently */
    for (port_end = 0; port_end < HOST_PORT_END_ARRAY_SIZE; port_end++)
    {
        host_port_endpoint_t *const endpoint = &context->host_ports[port_end];

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

        /* Allocation completion queue, sized for transmit and receive. */
        endpoint->cq = ibv_create_cq (endpoint->rdma_device, CQ_LENGTH, NULL, NULL, 0);
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

        /* Initialise the message buffer indices used for the endpoint */
        endpoint->next_tx_index = 0;
        endpoint->num_free_tx_buffers = NUM_PENDING_TX_FRAMES;
        endpoint->next_rx_index = 0;

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
                        .qp_type = IBV_QPT_UC
                    };

                    endpoint->qps[source_port_index][destination_port_index] = ibv_create_qp (endpoint->pd, &qp_init_attr);
                    CHECK_ASSERT (endpoint->qps[source_port_index][destination_port_index] != NULL);

                    /* Transition the Queue-Pair to Init state */
                    memset (&qp_attr, 0, sizeof (qp_attr));
                    qp_attr.qp_state = IBV_QPS_INIT;
                    qp_attr.pkey_index = 0;
                    qp_attr.port_num = endpoint->rdma_port_num;
                    qp_attr.qp_access_flags = 0;
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
                    host_port_endpoint_t *const send_endpoint = &context->host_ports[send_end];
                    host_port_endpoint_t *const recv_endpoint = &context->host_ports[recv_end];
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
                    qp_attr.dest_qp_num = recv_endpoint->qps[destination_port_index][source_port_index]->qp_num;
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

    /* Post receive work-requests for all message buffers */
    for (port_end = 0; port_end < HOST_PORT_END_ARRAY_SIZE; port_end++)
    {
        host_port_endpoint_t *const endpoint = &context->host_ports[port_end];

        do
        {
            post_endpoint_next_receive_wr (endpoint);
        } while (endpoint->next_rx_index != 0);
    }
}


/*
 * @brief Reset the statistics which are accumulated over one test interval
 * @param[in/out] statistics The statistics to reset
 */
static void reset_frame_test_statistics (frame_test_statistics_t *const statistics)
{
    for (frame_record_type_t frame_type = 0; frame_type < FRAME_RECORD_ARRAY_SIZE; frame_type++)
    {
        statistics->frame_counts[frame_type] = 0;
    }
    statistics->total_missing_frames = 0;
    for (uint32_t source_port_index = 0; source_port_index < NUM_DEFINED_PORTS; source_port_index++)
    {
        for (uint32_t destination_port_index = 0; destination_port_index < NUM_DEFINED_PORTS; destination_port_index++)
        {
            port_frame_statistics_t *const port_stats =
                    &statistics->port_frame_statistics[source_port_index][destination_port_index];

            port_stats->num_valid_rx_frames = 0;
            port_stats->num_missing_rx_frames = 0;
            port_stats->num_tx_frames = 0;
        }
    }
}


/**
 * @brief Initialise the context for the transmit/receive thread, for the start of the test
 * @param[out] context The initialised context
 */
static void transmit_receive_initialise (frame_tx_rx_thread_context_t *const context)
{
    open_host_endpoints (context);
    context->next_tx_sequence_number = 1;
    context->destination_tested_port_index = 0;
    context->source_port_offset = 1;
    context->host_tx_port = HOST_PORT_END_A;
    context->host_rx_port = HOST_PORT_END_B;

    /* Select how the host port direction is changed to minimise the number of flooded frames from the switch under test,
     * by minimising the number of times the switch sees an unknown destination MAC address as learns MAC addresses.
     * This is selected according to if are testing an odd or even number of ports, as determined by running the
     * ibv_uc_flooded_packets program. */
    const bool odd_number_of_ports_tested = (num_tested_port_indices % 1) == 1;
    context->host_port_direction_option = odd_number_of_ports_tested ?
            DIRECTION_REVERSE_EXCEPT_DESTINATION_TESTED_PORT_INDEX_RESETS : DIRECTION_REVERSE_EXCEPT_ALL_PORT_COMBINATIONS_TESTED;

    reset_frame_test_statistics (&context->statistics);
    context->statistics.max_pending_rx_frames = 0;
    context->statistics.final_statistics = false;

    if (arg_frame_debug_enabled)
    {
        /* Allocate space to record all expected frames within one test duration */
        const size_t nominal_records_per_test = 3; /* tx frame, copy of tx frame, rx frame */
        const size_t max_frame_rate = 82000; /* Slightly more than max non-jumbo frames can be sent on a 1 Gb link */
        context->frame_recording.allocated_length = max_frame_rate * nominal_records_per_test * (uint32_t) arg_test_interval_secs;
        context->frame_recording.frame_records = calloc
                (context->frame_recording.allocated_length, sizeof (context->frame_recording.frame_records[0]));
    }
    else
    {
        context->frame_recording.allocated_length = 0;
        context->frame_recording.frame_records = NULL;
    }
    context->frame_recording.num_frame_records = 0;

    const int64_t now = get_monotonic_time ();
    context->statistics.interval_start_time = now;
    context->test_interval_end_time = now + (arg_test_interval_secs * NSECS_PER_SEC);

    context->tx_rate_limited = arg_max_frame_rate_hz > 0;
    if (context->tx_rate_limited)
    {
        context->tx_interval = NSECS_PER_SEC / arg_max_frame_rate_hz;
        context->tx_time_of_next_frame = now;
    }

    /* Calculate the number of pending rx frames which can be stored per tested source / destination port combination.
     * This aims for a nomimal total number of pending rx frames divided among the the number of combinations tested. */
    const uint32_t num_tested_port_combinations = num_tested_port_indices * (num_tested_port_indices - 1);
    context->pending_rx_sequence_numbers_length = NOMINAL_TOTAL_PENDING_RX_FRAMES / num_tested_port_combinations;
    if (context->pending_rx_sequence_numbers_length < MIN_PENDING_RX_FRAMES_PER_PORT_COMBO)
    {
        context->pending_rx_sequence_numbers_length = MIN_PENDING_RX_FRAMES_PER_PORT_COMBO;
    }

    for (host_port_end_t port_end = 0; port_end < HOST_PORT_END_ARRAY_SIZE; port_end++)
    {
        host_port_endpoint_t *const endpoint = &context->host_ports[port_end];

        for (uint32_t source_port_index = 0; source_port_index < NUM_DEFINED_PORTS; source_port_index++)
        {
            for (uint32_t destination_port_index = 0; destination_port_index < NUM_DEFINED_PORTS; destination_port_index++)
            {
                pending_rx_frames_t *const pending = &endpoint->pending_rx_frames[source_port_index][destination_port_index];

                pending->pending_rx_sequence_numbers = NULL;
                pending->tx_frame_records = NULL;
                pending->num_pending_rx_frames = 0;
                pending->tx_index = 0;
                pending->rx_index = 0;
            }
        }

        /* Allocate space for pending rx frames for each tested source / destination port combination tested */
        const uint32_t pending_rx_sequence_numbers_pool_size =
                num_tested_port_combinations * context->pending_rx_sequence_numbers_length;
        uint32_t *const pending_rx_sequence_numbers_pool = calloc (pending_rx_sequence_numbers_pool_size, sizeof (uint32_t));
        frame_record_t **const tx_frame_records_pool = calloc (pending_rx_sequence_numbers_pool_size, sizeof (frame_record_t *));

        uint32_t pool_index = 0;
        for (uint32_t source_tested_port_index = 0; source_tested_port_index < num_tested_port_indices; source_tested_port_index++)
        {
            const uint32_t source_port_index = tested_port_indices[source_tested_port_index];

            for (uint32_t destination_tested_port_index = 0;
                 destination_tested_port_index < num_tested_port_indices;
                 destination_tested_port_index++)
            {
                const uint32_t destination_port_index = tested_port_indices[destination_tested_port_index];

                if (source_port_index != destination_port_index)
                {
                    pending_rx_frames_t *const pending = &endpoint->pending_rx_frames[source_port_index][destination_port_index];

                    pending->pending_rx_sequence_numbers = &pending_rx_sequence_numbers_pool[pool_index];
                    pending->tx_frame_records = &tx_frame_records_pool[pool_index];
                    pool_index += context->pending_rx_sequence_numbers_length;
                }
            }
        }
    }
}


/*
 * @brief When enabled by a command line option, record a transmit/receive frame for debug
 * @param[in/out] context Context to record the frame in
 * @param[in] frame_record The frame to record.
 * @return Returns a pointer to the recorded frame entry, or NULL if not recorded.
 *         Allows the caller to refer to the recorded frame for later updating the frame_missed field.
 */
static frame_record_t *record_frame_for_debug (frame_tx_rx_thread_context_t *const context,
                                               const frame_record_t *const frame_record)
{
    frame_record_t *recorded_frame = NULL;

    if (context->frame_recording.num_frame_records < context->frame_recording.allocated_length)
    {
        recorded_frame = &context->frame_recording.frame_records[context->frame_recording.num_frame_records];
        *recorded_frame = *frame_record;
        recorded_frame->frame_missed = false;
        context->frame_recording.num_frame_records++;
    }

    return recorded_frame;
}


/*
 * @brief Identify if an Ethernet frame, generated from a unreliable-connected message, is one used by the test.
 * @details If the Ethernet frame is one used by the test also extracts the source/destination port indices
 *          and the sequence number.
 * @param[in] context The context used to send/receive test frames.
 * @param[in] rx_end The host end used for the message receipt.
 * @param[in] wc When non-null the receive work-request completion.
 *               When NULL indicates are being called to record a transmitted test frame
 * @param[in] message The message to identify
 * @param[out] frame_record Contains information for the identified frame.
 *                          For a receive frame haven't yet performed the checks against the pending receive frames.
 */
static void identify_frame (const frame_tx_rx_thread_context_t *const context,
                            const host_port_end_t rx_end,
                            const struct ibv_wc *const wc, const uc_test_message_t *const message,
                            frame_record_t *const frame_record)
{
    const host_port_end_t tx_end = (rx_end == HOST_PORT_END_A) ? HOST_PORT_END_B : HOST_PORT_END_A;

    if (wc != NULL)
    {
        /* Use the len from the received frame */
        frame_record->byte_len = wc->byte_len;
    }
    else
    {
        /* Set the len for the transmitted frame */
        frame_record->byte_len = sizeof (uc_test_message_t);
    }

    frame_record->relative_test_time = get_monotonic_time () - context->statistics.interval_start_time;

    /* Determine if the frame is one generated by the test program, validating the message contents */
    bool is_test_frame = frame_record->byte_len >= sizeof (uc_test_message_t);

    if (is_test_frame)
    {
        is_test_frame = (message->source_port_index < NUM_DEFINED_PORTS) &&
                (message->destination_port_index < NUM_DEFINED_PORTS);

        /* For a receive frame verify the source and destination Queue-Pair numbers match that for the source and destination
         * port indices, as a way of checking the message and was received on the expected Queue-Pair and thus VLAN. */
        if (is_test_frame && (wc != NULL))
        {
            const host_port_endpoint_t *const tx_endpoint = &context->host_ports[tx_end];
            const host_port_endpoint_t *const rx_endpoint = &context->host_ports[rx_end];
            const struct ibv_qp *const tx_qp = tx_endpoint->qps[message->source_port_index][message->destination_port_index];
            const struct ibv_qp *const rx_qp = rx_endpoint->qps[message->source_port_index][message->destination_port_index];

            is_test_frame = (tx_qp != NULL) && (message->source_qp_num == tx_qp->qp_num) &&
                    (rx_qp != NULL) && (message->destination_qp_num == rx_qp->qp_num);
        }
    }

    /* Set the initial identified frame type. FRAME_RECORD_RX_TEST_FRAME may be modified following
     * subsequent checks against the pending receive frames */
    if (is_test_frame)
    {
        frame_record->source_port_index = message->source_port_index;
        frame_record->destination_port_index = message->destination_port_index;
        frame_record->tx_end = tx_end;
        frame_record->test_sequence_number = message->test_sequence_number;
        frame_record->frame_type = (wc != NULL) ? FRAME_RECORD_RX_TEST_FRAME : FRAME_RECORD_TX_TEST_FRAME;
    }
    else
    {
        frame_record->frame_type = FRAME_RECORD_RX_OTHER;
    }
}


/*
 * @brief Called when a received frame has been identified as a test frame, to update the list of pending frames
 * @param[in/out] context Context to update the pending frames for
 * @param[in] rx_end The host end used for the message receipt.
 * @param[in/out] frame_record The received frame to compare against the list of pending frames.
 *                             On output the frame_type has been updated to identify which sub-type of receive frame it is.
 */
static void handle_pending_rx_frame (frame_tx_rx_thread_context_t *const context, const host_port_end_t rx_end,
                                     frame_record_t *const frame_record)
{
    host_port_endpoint_t *const endpoint = &context->host_ports[rx_end];
    pending_rx_frames_t *const pending =
            &endpoint->pending_rx_frames[frame_record->source_port_index][frame_record->destination_port_index];
    port_frame_statistics_t *const port_stats =
            &context->statistics.port_frame_statistics[frame_record->source_port_index][frame_record->destination_port_index];

    bool pending_match_found = false;
    while ((!pending_match_found) && (pending->num_pending_rx_frames > 0))
    {
        if (frame_record->test_sequence_number == pending->pending_rx_sequence_numbers[pending->rx_index])
        {
            /* This is an expected pending receive frame */
            port_stats->num_valid_rx_frames++;
            frame_record->frame_type = FRAME_RECORD_RX_TEST_FRAME;
            pending_match_found = true;
        }
        else
        {
            /* The sequence number is not the next expected pending, which means a preceding frame has been missed */
            port_stats->num_missing_rx_frames++;
            context->statistics.total_missing_frames++;
            if (pending->tx_frame_records[pending->rx_index] != NULL)
            {
                pending->tx_frame_records[pending->rx_index]->frame_missed = true;
            }
        }

        pending->num_pending_rx_frames--;
        pending->tx_frame_records[pending->rx_index] = NULL;
        pending->rx_index = (pending->rx_index + 1) % context->pending_rx_sequence_numbers_length;
    }

    if (!pending_match_found)
    {
        frame_record->frame_type = FRAME_RECORD_RX_UNEXPECTED_FRAME;
    }
}


/**
 * @brief Poll for all transmit or receive work-request completions, emptying the completion queues.
 * @details By emptying the completion queues this gives completion to processing received frames over transmitting,
 *          to try and avoid received frames being missed due to no posted receive work-request.
 * @param[in,out] context The context to poll for completions.
 */
static void poll_for_tx_or_rx_completions (frame_tx_rx_thread_context_t *const context)
{
    int num_completions;
    frame_record_t frame_record;

    for (host_port_end_t port_end = 0; port_end < HOST_PORT_END_ARRAY_SIZE; port_end++)
    {
        host_port_endpoint_t *const endpoint = &context->host_ports[port_end];

        num_completions = ibv_poll_cq (endpoint->cq, CQ_LENGTH, endpoint->wcs);
        CHECK_ASSERT (num_completions >= 0);
        for (int completion_index = 0; completion_index < num_completions; completion_index++)
        {
            const struct ibv_wc *const wc = &endpoint->wcs[completion_index];

            /* Since connected unreliable Queue-Pairs are used don't expect a non-successful completion status even if
             * test frames are missed. If the work-request fails the Queue-Pair could have transitioned to the error
             * state so abort if a non-successful status and report diagnostic information. */
            if (wc->status != IBV_WC_SUCCESS)
            {
                console_printf ("%s port %" PRIu8 " work-request ID %" PRIu64 " failed with %s\n",
                        endpoint->rdma_device->device->name, endpoint->rdma_port_num, wc->wr_id, ibv_wc_status_str (wc->status));
                exit (EXIT_FAILURE);
            }

            switch (wc->wr_id)
            {
            case RX_WR_ID:
                /* Process received message */
                identify_frame (context, port_end, wc, &endpoint->test_messages.rx_messages[endpoint->next_rx_index],
                        &frame_record);
                if (frame_record.frame_type != FRAME_RECORD_RX_OTHER)
                {
                    handle_pending_rx_frame (context, port_end, &frame_record);
                }
                context->statistics.frame_counts[frame_record.frame_type]++;
                record_frame_for_debug (context, &frame_record);

                /* After the receive message buffer has been processed, re-post a receive work-request for it */
                post_endpoint_next_receive_wr (endpoint);
                break;

            case TX_WR_ID:
                /* Just check a transmit completion was pending, and indicate no longer pending */
                CHECK_ASSERT (endpoint->num_free_tx_buffers < NUM_PENDING_TX_FRAMES);
                endpoint->num_free_tx_buffers++;
                break;

            default:
                check_assert (false, "Unexpected wr_id %" PRIu64, wc->wr_id);
                break;
            }
        }
    }
}


/*
 * @brief Sequence transmitting the next test frame, cycling around combinations of the source and destination ports
 * @details This also records the frame as pending receipt, identified with the combination of source/destination port
 *          and sequence number.
 * @param[in/out] context Context used transmitting frames.
 */
static void transmit_next_test_frame (frame_tx_rx_thread_context_t *const context)
{
    int rc;
    const uint32_t source_tested_port_index =
            (context->destination_tested_port_index + context->source_port_offset) % num_tested_port_indices;
    const uint32_t destination_port_index = tested_port_indices[context->destination_tested_port_index];
    const uint32_t source_port_index = tested_port_indices[source_tested_port_index];
    host_port_endpoint_t *const tx_endpoint = &context->host_ports[context->host_tx_port];
    host_port_endpoint_t *const rx_endpoint = &context->host_ports[context->host_rx_port];
    struct ibv_qp *const tx_qp = tx_endpoint->qps[source_port_index][destination_port_index];
    struct ibv_qp *const rx_qp = rx_endpoint->qps[source_port_index][destination_port_index];
    pending_rx_frames_t *const pending = &rx_endpoint->pending_rx_frames[source_port_index][destination_port_index];
    port_frame_statistics_t *const port_stats =
            &context->statistics.port_frame_statistics[source_port_index][destination_port_index];
    uc_test_message_t *const tx_message = &tx_endpoint->test_messages.tx_messages[tx_endpoint->next_tx_index];

    /* Populate the test message and queue it for transmission */
    tx_message->test_sequence_number = context->next_tx_sequence_number;
    tx_message->source_port_index = source_port_index;
    tx_message->destination_port_index = destination_port_index;
    tx_message->source_qp_num = tx_qp->qp_num;
    tx_message->destination_qp_num = rx_qp->qp_num;

    struct ibv_sge sg_entry =
    {
        .addr = (uint64_t) tx_message,
        .length = sizeof (uc_test_message_t),
        .lkey = tx_endpoint->mr->lkey
    };
    struct ibv_send_wr send_wr =
    {
        .wr_id = TX_WR_ID,
        .next = NULL,
        .sg_list = &sg_entry,
        .num_sge = 1,
        .opcode = IBV_WR_SEND,
        .send_flags = IBV_SEND_SIGNALED
    };
    struct ibv_send_wr *bad_wr;

    CHECK_ASSERT (tx_endpoint->num_free_tx_buffers > 0);
    rc = ibv_post_send (tx_qp, &send_wr, &bad_wr);
    CHECK_ASSERT (rc == 0);
    tx_endpoint->num_free_tx_buffers--;

    /* When debug is enabled identify the transmit frame and record it */
    frame_record_t *recorded_frame = NULL;
    if (arg_frame_debug_enabled)
    {
        frame_record_t frame_record;

        identify_frame (context, context->host_rx_port, NULL, tx_message, &frame_record);
        recorded_frame = record_frame_for_debug (context, &frame_record);
    }

    /* Update transmit frame counts */
    context->statistics.frame_counts[FRAME_RECORD_TX_TEST_FRAME]++;
    port_stats->num_tx_frames++;

    /* If the maximum number of receive frames are pending, then mark the oldest as missing */
    if (pending->num_pending_rx_frames == context->pending_rx_sequence_numbers_length)
    {
        port_stats->num_missing_rx_frames++;
        context->statistics.total_missing_frames++;
        pending->num_pending_rx_frames--;
        if (pending->tx_frame_records[pending->rx_index] != NULL)
        {
            pending->tx_frame_records[pending->rx_index]->frame_missed = true;
        }
        pending->rx_index = (pending->rx_index + 1) % context->pending_rx_sequence_numbers_length;
    }

    /* Record the transmitted frame as pending receipt */
    pending->pending_rx_sequence_numbers[pending->tx_index] = context->next_tx_sequence_number;
    pending->tx_frame_records[pending->tx_index] = recorded_frame;
    pending->tx_index = (pending->tx_index + 1) % context->pending_rx_sequence_numbers_length;
    pending->num_pending_rx_frames++;
    if (pending->num_pending_rx_frames > context->statistics.max_pending_rx_frames)
    {
        context->statistics.max_pending_rx_frames = pending->num_pending_rx_frames;
    }

    /* Advance to the next frame which will be transmitted, and determine when to swap the host ports */
    bool swap_host_ports = true;
    tx_endpoint->next_tx_index = (tx_endpoint->next_tx_index + 1) % NUM_PENDING_TX_FRAMES;
    context->next_tx_sequence_number++;
    context->destination_tested_port_index = (context->destination_tested_port_index + 1) % num_tested_port_indices;
    if (context->destination_tested_port_index == 0)
    {
        if (context->host_port_direction_option == DIRECTION_REVERSE_EXCEPT_DESTINATION_TESTED_PORT_INDEX_RESETS)
        {
            swap_host_ports = false;
        }
        context->source_port_offset++;
        if (context->source_port_offset == num_tested_port_indices)
        {
            context->source_port_offset = 1;
            if (context->host_port_direction_option == DIRECTION_REVERSE_EXCEPT_ALL_PORT_COMBINATIONS_TESTED)
            {
                swap_host_ports = false;
            }
        }
    }

    if (swap_host_ports)
    {
        const uint32_t current_host_rx_port = context->host_rx_port;

        context->host_rx_port = context->host_tx_port;
        context->host_tx_port = current_host_rx_port;
    }
}


/*
 * @brief Thread which transmits test frames and checks for receipt of the frames from the switch under test
 * @param[out] arg The context for the thread.
 */
static void *transmit_receive_thread (void *arg)
{
    frame_tx_rx_thread_context_t *const context = arg;
    bool exit_requested = false;
    int64_t now;
    int rc;

    transmit_receive_initialise (context);

    /* Run test until requested to exit.
     * This gives preference to polling for receipt of test frames, and when no available frame transmits the next test frame.
     * This tries to send frames at the maximum possible rate, and relies upon the poll for frame receipt not causing any
     * frames to be discarded by ensuring receive work-requests are kept posted. */
    while (!exit_requested)
    {
        now = get_monotonic_time ();

        /* Poll for completion, which processes received test frames */
        poll_for_tx_or_rx_completions (context);

        if (context->host_ports[context->host_tx_port].num_free_tx_buffers == 0)
        {
            /* All transmit buffers in use, waiting for completion to be signalled before the next transmit can occur */
        }
        else if (!context->tx_rate_limited)
        {
            /* Transmit frames as quickly as possible */
            transmit_next_test_frame (context);
        }
        else if (now >= context->tx_time_of_next_frame)
        {
            /* Transmit frames, with the rate limited to a maximum */
            transmit_next_test_frame (context);
            context->tx_time_of_next_frame += context->tx_interval;
        }

        if (now > context->test_interval_end_time)
        {
            /* The end of test interval has been reached */
            context->statistics.interval_end_time = now;
            if (arg_frame_debug_enabled)
            {
                exit_requested = true;
            }
            else if (test_stop_requested)
            {
                exit_requested = true;
            }

            /* Publish and then reset statistics for the next test interval */
            rc = sem_wait (&test_statistics_free);
            CHECK_ASSERT (rc == 0);
            context->statistics.final_statistics = exit_requested;
            test_statistics = context->statistics;
            rc = sem_post (&test_statistics_populated);
            CHECK_ASSERT (rc == 0);
            reset_frame_test_statistics (&context->statistics);
            context->statistics.interval_start_time = context->statistics.interval_end_time;
            context->test_interval_end_time += (arg_test_interval_secs * NSECS_PER_SEC);
        }
    }

    return NULL;
}


/**
 * @brief Write a CSV file which contains a record of the frames sent/received during a test.
 * @details This is used to debug a single test interval.
 * @param[in] frame_debug_csv_filename Name of CSV file to create
 * @param[in] frame_recording The frames which were sent/received during the test
 */
static void write_frame_debug_csv_file (const char *const frame_debug_csv_filename, const frame_records_t *const frame_recording)
{
    /* Create CSV file and write headers */
    FILE *const csv_file = fopen (frame_debug_csv_filename, "w");
    if (csv_file == NULL)
    {
        console_printf ("Failed to create %s\n", frame_debug_csv_filename);
        exit (EXIT_FAILURE);
    }
    fprintf (csv_file, "frame type,relative test time (secs),Tx end,missed,source switch port,destination switch port,len,test sequence number\n");

    /* Write one row per recorded frame */
    for (uint32_t frame_index = 0; frame_index < frame_recording->num_frame_records; frame_index++)
    {
        const frame_record_t *const frame_record = &frame_recording->frame_records[frame_index];

        fprintf (csv_file, "%s,%.6f,%s,%s",
                frame_record_types[frame_record->frame_type],
                frame_record->relative_test_time / 1E9,
                host_port_ends[frame_record->tx_end],
                frame_record->frame_missed ? "Frame missed" : "");
        if (frame_record->frame_type != FRAME_RECORD_RX_OTHER)
        {
            fprintf (csv_file, ",%" PRIu32 ",%" PRIu32,
                    test_ports[frame_record->source_port_index].switch_port_number,
                    test_ports[frame_record->destination_port_index].switch_port_number);
        }
        else
        {
            fprintf (csv_file, ",,");
        }
        fprintf (csv_file, ",%" PRIu32, frame_record->byte_len);
        if (frame_record->frame_type != FRAME_RECORD_RX_OTHER)
        {
            fprintf (csv_file, ",%" PRIu32, frame_record->test_sequence_number);
        }
        else
        {
            fprintf (csv_file, ",");
        }
        fprintf (csv_file, "\n");
    }

    fclose (csv_file);
}


/*
 * @brief Write the frame test statistics from the most recent test interval.
 * @details This is written as:
 *          - The console with a overall summary of which combinations of source/destination ports have missed frames.
 *          - A CSV file which has the per-port count of frames.
 * @param[in/out] results_summary Used to maintain a summary of which test intervals have had test failures.
 * @param[in] statistics The statistics crom the most recent interval
 */
static void write_frame_test_statistics (results_summary_t *const results_summary,
                                         const frame_test_statistics_t *const statistics)
{
    uint32_t source_tested_port_index;
    uint32_t destination_tested_port_index;
    char time_str[80];
    struct tm broken_down_time;
    struct timeval tod;
    frame_record_type_t frame_type;

    /* Display time when these statistics are reported */
    gettimeofday (&tod, NULL);
    const time_t tod_sec = tod.tv_sec;
    const int64_t tod_msec = tod.tv_usec / 1000;
    localtime_r (&tod_sec, &broken_down_time);
    strftime (time_str, sizeof (time_str), "%H:%M:%S", &broken_down_time);
    size_t str_len = strlen (time_str);
    snprintf (&time_str[str_len], sizeof (time_str) - str_len, ".%03" PRIi64, tod_msec);

    console_printf ("\n%s\n", time_str);

    /* Print header for counts */
    const int count_field_width = 13;
    for (frame_type = 0; frame_type < FRAME_RECORD_ARRAY_SIZE; frame_type++)
    {
        console_printf ("%*s  ", count_field_width, frame_record_types[frame_type]);
    }
    console_printf ("%*s  %*s\n", count_field_width, "missed frames", count_field_width, "tx rate (Hz)");

    /* Display the count of the different frame types during the test interval.
     * Even when no missing frames the count of the transmit and receive frames may be different due to frames
     * still in flight at the end of the test interval. */
    for (frame_type = 0; frame_type < FRAME_RECORD_ARRAY_SIZE; frame_type++)
    {
        console_printf ("%*" PRIu32 "  ", count_field_width, statistics->frame_counts[frame_type]);
    }

    /* Report the total number of missing frames during the test interval */
    console_printf ("%*" PRIu32 "  ", count_field_width, statistics->total_missing_frames);

    /* Report the average frame rate achieved over the statistics interval */
    const double statistics_interval_secs = (double) (statistics->interval_end_time - statistics->interval_start_time) / 1E9;
    console_printf ("%*.1f\n", count_field_width,
            (double) statistics->frame_counts[FRAME_RECORD_TX_TEST_FRAME] / statistics_interval_secs);

    /* Display summary of missed frames over combination of source / destination ports */
    console_printf ("\nSummary of missed frames : '.' none missed 'S' some missed 'A' all missed\n");
    console_printf ("Source  Destination ports --->\n");
    for (uint32_t header_row = 0; header_row < 2; header_row++)
    {
        console_printf ("%s", (header_row == 0) ? "  port  " : "        ");
        for (destination_tested_port_index = 0;
             destination_tested_port_index < num_tested_port_indices;
             destination_tested_port_index++)
        {
            char port_num_text[3];

            snprintf (port_num_text, sizeof (port_num_text), "%2" PRIu32,
                    test_ports[tested_port_indices[destination_tested_port_index]].switch_port_number);
            console_printf ("%c", port_num_text[header_row]);
        }
        console_printf ("\n");
    }

    for (source_tested_port_index = 0; source_tested_port_index < num_tested_port_indices; source_tested_port_index++)
    {
        const uint32_t source_port_index = tested_port_indices[source_tested_port_index];

        console_printf ("    %2" PRIu32 "  ", test_ports[source_port_index].switch_port_number);
        for (destination_tested_port_index = 0;
             destination_tested_port_index < num_tested_port_indices;
             destination_tested_port_index++)
        {
            const uint32_t destination_port_index = tested_port_indices[destination_tested_port_index];
            const port_frame_statistics_t *const port_statistics =
                    &statistics->port_frame_statistics[source_port_index][destination_port_index];
            char port_status;

            if (source_port_index == destination_port_index)
            {
                port_status = ' ';
            }
            else if (port_statistics->num_missing_rx_frames == 0)
            {
                port_status = '.';
            }
            else if (port_statistics->num_valid_rx_frames > 0)
            {
                port_status = 'S';
            }
            else
            {
                port_status = 'A';
            }
            console_printf ("%c", port_status);
        }
        console_printf ("\n");
    }

    /* Any missed frames counts as a test failure */
    if (statistics->total_missing_frames > 0)
    {
        results_summary->num_test_intervals_with_failures++;
        snprintf (results_summary->time_of_last_failure, sizeof (results_summary->time_of_last_failure), "%s", time_str);
    }

    /* Create per-port counts CSV file on first call, and write column headers */
    if (results_summary->per_port_counts_csv_file == NULL)
    {
        results_summary->per_port_counts_csv_file = fopen (results_summary->per_port_counts_csv_filename, "w");
        if (results_summary->per_port_counts_csv_file == NULL)
        {
            console_printf ("Failed to create %s\n", results_summary->per_port_counts_csv_filename);
            exit (EXIT_FAILURE);
        }
        fprintf (results_summary->per_port_counts_csv_file,
                "Time,Source switch port,Destination switch port,Num tx frames,Num valid rx frames,Num missing rx frames\n");
    }

    /* Write one row containing the number of frames per combination of source and destination switch ports tested */
    for (source_tested_port_index = 0; source_tested_port_index < num_tested_port_indices; source_tested_port_index++)
    {
        for (destination_tested_port_index = 0;
             destination_tested_port_index < num_tested_port_indices;
             destination_tested_port_index++)
        {
            if (source_tested_port_index != destination_tested_port_index)
            {
                const uint32_t source_port_index = tested_port_indices[source_tested_port_index];
                const uint32_t destination_port_index = tested_port_indices[destination_tested_port_index];
                const port_frame_statistics_t *const port_statistics =
                        &statistics->port_frame_statistics[source_port_index][destination_port_index];

                fprintf (results_summary->per_port_counts_csv_file,
                        " %s,%" PRIu32 ",%" PRIu32 ",%" PRIu32 ",%" PRIu32 ",%" PRIu32 "\n",
                        time_str,
                        test_ports[source_port_index].switch_port_number,
                        test_ports[destination_port_index].switch_port_number,
                        port_statistics->num_tx_frames,
                        port_statistics->num_valid_rx_frames,
                        port_statistics->num_missing_rx_frames);
            }
        }
    }

    /* Display overall summary of test failures */
    console_printf ("Total test intervals with failures = %" PRIu32, results_summary->num_test_intervals_with_failures);
    if (results_summary->num_test_intervals_with_failures)
    {
        console_printf (" : last failure %s\n", (statistics->total_missing_frames > 0) ? "NOW" : results_summary->time_of_last_failure);
    }
    else
    {
        console_printf ("\n");
    }
}


int main (int argc, char *argv[])
{
    int rc;

    /* Read the command line arguments */
    read_command_line_arguments (argc, argv);

    /* Add protection against fork() being called */
    rc = ibv_fork_init ();
    CHECK_ASSERT (rc == 0);

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    /* Initialise the semaphores used to control access to the test interval statistics */
    rc = sem_init (&test_statistics_free, 0, 1);
    CHECK_ASSERT (rc == 0);
    rc = sem_init (&test_statistics_populated, 0, 0);
    CHECK_ASSERT (rc == 0);

    /* Set filenames which contain the output files containing the date/time and OS used  */
    results_summary_t results_summary = {{0}};
    const time_t tod_now = time (NULL);
    struct tm broken_down_time;
    char date_time_str[80];
    char frame_debug_csv_filename[128];
    char console_filename[128];

    localtime_r (&tod_now, &broken_down_time);
    strftime (date_time_str, sizeof (date_time_str), "%Y%m%dT%H%M%S", &broken_down_time);
    snprintf (frame_debug_csv_filename, sizeof (frame_debug_csv_filename), "%s_frames_debug_%s.csv", date_time_str, OS_NAME);
    snprintf (console_filename, sizeof (console_filename), "%s_console_%s.txt", date_time_str, OS_NAME);
    snprintf (results_summary.per_port_counts_csv_filename, sizeof (results_summary.per_port_counts_csv_filename),
            "%s_per_port_counts_%s.csv", date_time_str, OS_NAME);

    console_file = fopen (console_filename, "wt");
    if (console_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", console_filename);
        exit (EXIT_FAILURE);
    }

    /* Report the command line arguments used */
    console_printf ("Writing per-port counts to %s\n", results_summary.per_port_counts_csv_filename);
    console_printf ("Using RDMA interfaces %s port %" PRIu8 " <-> %s %" PRIu8 "\n",
            arg_rdma_device_names[HOST_PORT_END_A], arg_rdma_ports[HOST_PORT_END_A],
            arg_rdma_device_names[HOST_PORT_END_B], arg_rdma_ports[HOST_PORT_END_B]);
    console_printf ("Test interval = %" PRIi64 " (secs)\n", arg_test_interval_secs);
    console_printf ("Frame debug enabled = %s\n", arg_frame_debug_enabled ? "Yes" : "No");
    if (arg_max_frame_rate_hz > 0)
    {
        console_printf ("Frame transmit max rate = %" PRIi64 " Hz\n", arg_max_frame_rate_hz);
    }
    else
    {
        console_printf ("Frame transmit max rate = None\n");
    }

    /* Create the transmit_receive_thread */
    frame_tx_rx_thread_context_t *const tx_rx_thread_context = calloc (1, sizeof (*tx_rx_thread_context));
    pthread_t tx_rx_thread_handle;

    rc = pthread_create (&tx_rx_thread_handle, NULL, transmit_receive_thread, tx_rx_thread_context);
    CHECK_ASSERT (rc == 0);

    /* Report that the test has started */
    if (arg_frame_debug_enabled)
    {
        console_printf ("Running for a single test interval to collect debug information\n");
    }
    else
    {
#ifdef _WIN32
        signal (SIGINT, stop_test_handler);
#else
        struct sigaction action;

        memset (&action, 0, sizeof (action));
        action.sa_handler = stop_test_handler;
        action.sa_flags = SA_RESTART;
        rc = sigaction (SIGINT, &action, NULL);
        if (rc != 0)
        {
            console_printf ("sigaction() failed rc=%d\n", rc);
            exit (EXIT_FAILURE);
        }
#endif
        console_printf ("Press Ctrl-C to stop test at end of next test interval\n");
    }

    /* Report the statistics for each test interval, stopping when get the final statistics */
    bool exit_requested = false;
    while (!exit_requested)
    {
        /* Wait for the statistics upon completion of a test interval */
        rc = sem_wait (&test_statistics_populated);
        CHECK_ASSERT (rc == 0);

        /* Report the statistics */
        write_frame_test_statistics (&results_summary, &test_statistics);
        exit_requested = test_statistics.final_statistics;

        /* Indicate the main thread has completed using the test_statistics */
        rc = sem_post (&test_statistics_free);
        CHECK_ASSERT (rc == 0);
    }

    /* Wait for the transmit_receive_thread to exit */
    rc = pthread_join (tx_rx_thread_handle, NULL);
    CHECK_ASSERT (rc == 0);

    console_printf ("Max pending rx frames = %" PRIu32 " out of %" PRIu32 "\n",
            tx_rx_thread_context->statistics.max_pending_rx_frames, tx_rx_thread_context->pending_rx_sequence_numbers_length);

    /* Write the debug frame recording information if enabled */
    if (arg_frame_debug_enabled)
    {
        write_frame_debug_csv_file (frame_debug_csv_filename, &tx_rx_thread_context->frame_recording);
    }

    fclose (results_summary.per_port_counts_csv_file);
    fclose (console_file);

    return EXIT_SUCCESS;
}
