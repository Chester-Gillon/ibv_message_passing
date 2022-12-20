/*
 * @file dpdk_switch_test.c
 * @date 11 Dec 2022
 * @author Chester Gillon
 * @details A variant of the https://github.com/chester-gillon/switch_test but which uses
 *          Data Plane Development Kit (DPDK)to transmit and receive the frames, rather than PCAP.
 * @todo console_printf() doesn't write any console output from the DPDK EAL, e.g. errors, to the log file.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <math.h>

#include <unistd.h>
#include <limits.h>
#include <semaphore.h>
#include <sys/mman.h>

#include <rte_eal.h>
#include <rte_lcore.h>
#include <rte_ethdev.h>


/* Define a string to report the Operating System just to report in result filenames, for commonality with the
 * pcap based program. Fixed to linux as haven't attempted to use DPDK under Windows. */
#define OS_NAME "linux"


#define NSECS_PER_SEC 1000000000LL

/* Ethernet frame types */
#define ETH_P_8021Q    0x8100
#define ETH_P_ETHERCAT 0x88a4


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


/** Define a locally administrated MAC address and VLAN for each possible switch port under test.
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


/* Only need a single device queue for this program */
#define NUM_RX_QUEUES 1
#define NUM_TX_QUEUES 1
#define QUEUE_ID      0


/* Array which defines which of the indices in test_ports[] are tested at run-time.
 * This allows command line arguments to specify a sub-set of the possible ports to be tested. */
static uint32_t tested_port_indices[NUM_DEFINED_PORTS];
static uint32_t num_tested_port_indices;


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


/* The total number of bit times one ethercat_frame_t occupies a network port for */
#define TEST_PACKET_TOTAL_OCTETS (7 + /* Preamble */ \
                                  1 + /* Start frame delimiter */ \
                                  sizeof (ethercat_frame_t) + \
                                  4 + /* Frame check sequence */ \
                                  12) /* Interpacket gap */
#define BITS_PER_OCTET 8
#define TEST_PACKET_BITS (TEST_PACKET_TOTAL_OCTETS * BITS_PER_OCTET)


/* Identifies one type of frame recorded by the test */
typedef enum
{
    /* A frame transmitted by the test */
    FRAME_RECORD_TX_TEST_FRAME,
    /* An EtherCAT test frame in the format which is transmitted, and which was received by the test on the
     * VLAN for the expected destination port. This frame contains an expected pending sequence number. */
    FRAME_RECORD_RX_TEST_FRAME,
    /* As per FRAME_RECORD_RX_TEST_FRAME except the received sequence number was not expected.
     * This may happen if frames get delayed such that there is no recording of the pending sequence number. */
    FRAME_RECORD_RX_UNEXPECTED_FRAME,
    /* An EtherCAT test frame in the format which is transmitted, and which was received by the test on a
     * VLAN other than that expected for the destination port.
     * This means the frame was flooded because the switch under test didn't know which port the destination MAC address
     * was for. */
    FRAME_RECORD_RX_FLOODED_FRAME,
    /* A frame received by the test, which isn't one transmitted.
       I.e. any frames which are not generated by the test program.
       While ibv_create_flow() could be used to apply a IBV_FLOW_SPEC_ETH filter to only receive EtherCAT frames
       consider it better to receive all frames so can report statistics on the number of other frames which are
       may be consuming some bandwidth on the switch ports under test. */
    FRAME_RECORD_RX_OTHER,

    FRAME_RECORD_ARRAY_SIZE
} frame_record_type_t;


/* Look up table which gives the description of each frame_record_type_t */
static const char *const frame_record_types[FRAME_RECORD_ARRAY_SIZE] =
{
    [FRAME_RECORD_TX_TEST_FRAME      ] = "Tx Test",
    [FRAME_RECORD_RX_TEST_FRAME      ] = "Rx Test",
    [FRAME_RECORD_RX_UNEXPECTED_FRAME] = "Rx Unexpected",
    [FRAME_RECORD_RX_FLOODED_FRAME   ] = "Rx Flooded",
    [FRAME_RECORD_RX_OTHER           ] = "Rx Other"
};


/* Used to record one frame transmitted or received during the test, for debugging purposes */
typedef struct
{
    /* Identifies the type of frame */
    frame_record_type_t frame_type;
    /* The relative time from the start of the test that the frame was sent or received.
       This is using the monotonic time used by the test busy-polling loop rather than a DPDK timestamp offload
       (which might not available for all DPDK PMDs). */
    int64_t relative_test_time;
    /* The destination and source MAC addresses from the frame */
    uint8_t destination_mac_addr[ETHER_MAC_ADDRESS_LEN];
    uint8_t source_mac_addr[ETHER_MAC_ADDRESS_LEN];
    /* The length of the frame */
    uint32_t len;
    /* The ether type of the frame, the field this is extracted from depends upon if there is a VLAN */
    uint16_t ether_type;
    /* True if the frame was for a VLAN */
    bool vlan_present;
    /* When vlan_present is true identifies the VLAN this was for */
    uint16_t vlan_id;
    /* When frame_type is other than FRAME_RECORD_RX_OTHER the sequence number of the frame.
     * Allows received frames to be matched against the transmitted frame. */
    uint32_t test_sequence_number;
    /* When frame_type is other than FRAME_RECORD_RX_OTHER the source and destination port numbers of the frame,
     * based upon matching the source_mac_addr and destination_mac_addr */
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


/* Command line argument which specifies the requested rate on each tested switch port, in Mbps */
#define DEFAULT_TESTED_PORT_MBPS 100.0
static double arg_tested_port_mbps = DEFAULT_TESTED_PORT_MBPS;


/* Command line argument which controls how the test runs:
 * - When false the test runs until requested to stop, and only reports summary information for each test interval.
 * - When true the test runs for a single test interval, recording the transmitted/received frames in memory which are written
 *   to s CSV file at the end of the test interval. */
static bool arg_frame_debug_enabled = false;


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
     * Done this way round to avoid marking pending frames at the end of a test sequence as missing. */
    frame_record_t **tx_frame_records;
} pending_rx_frames_t;


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


/* The context used for the thread which sends/receive the test frames */
typedef struct
{
    /* The identity of the lcore used to send/receive the test frames */
    int worker_lcore_id;
    /* The DPDK port identity (aka device) used to send/receive test frames, which is extracted from the EAL options.
     *  rte_eal_init() will have probed the device. */
    uint16_t port_id;
    /* The DPDK device information for port_id */
    struct rte_eth_dev_info dev_info;
    /* The RDMA device used to send/receive frames */
    //@todo change for DPDK struct ibv_context *rdma_device;
    /* Attributes for rdma_device and the selected port */
    //@todo change for DPDK struct ibv_device_attr_ex device_attributes;
    //@todo change for DPDK struct ibv_port_attr port_attributes;
    /* The thread domain used */
    //@todo change for DPDK struct ibv_td *td;
    /* The protection domain used */
    //@todo change for DPDK struct ibv_pd *protection_domain;
    /* The parent domain used */
    //@todo change for DPDK struct ibv_pd *parent_domain;
    /* The completion queue used */
    //@todo change for DPDK struct ibv_cq_ex *cq;
    /* The queue-pair queue used, with structures for different APIs */
    //@todo change for DPDK struct ibv_qp *qp;
    //@todo change for DPDK struct ibv_qp_ex *qpx;
    /* Receive flow used to receive raw all raw Ethernet packets */
    //@todo change for DPDK struct ibv_flow *flow;
    /* The number of DPDK descriptors allocated for transmit and receive.
     * These are adjusted to fit the limits imposed for the Ethernet device. */
    uint16_t tx_num_descriptors;
    uint16_t rx_num_descriptors;
    /* Pool used for transmit descriptors */
    struct rte_mempool *tx_mbuf_pool;
    /* Pool used for receive descriptors */
    struct rte_mempool *rx_mbuf_pool;
    /* Array of length [rx_num_descriptors] used for receiving packets */
    struct rte_mbuf **rx_pkts;
    /* Used to allocate space for tx_num_buffers+rx_num_buffers for transmitting and receiving frames by RDMA.
     * Initially haven't attempted to align the start of each frame to a cache line as not sure if will improve
     * performance without measuring it. */
    //@todo DPDK mempool's are used ethercat_frame_t *frames;
    /* The RDMA memory region to access the frames[] array */
    //@todo change for DPDK struct ibv_mr *mr;
    /* Circular buffer of length [tx_num_buffers] for queueing frames for transmit via RDMA */
    //@todo DPDK mbufs are used ethercat_frame_t *tx_frames;
    /* The index into tx_frames[] to be used for the next frame transmitted */
    //@todo DPDK mbufs are used so no index maintained uint32_t next_tx_buffer_index;
    /* The number of entries in tx_frames[] which have been queued for transmission and waiting for completion.
     * When num_tx_buffers_queued==tx_num_buffers transmission has to pause waiting for a completion to be signaled. */
    //@todo DPDK doesn't allow this to be tracked with all drivers uint32_t num_tx_buffers_queued;
    /* Circular buffer of length [rx_num_buffers] for posting receive requests via RDMA */
    //@todo DPDK mbufs are used ethercat_frame_t *rx_frames;
    /* The index into rx_frames[] to be used for the next frame received */
    //@todo DPDK mbufs are used so no index maintained uint32_t next_rx_buffer_index;
    /* The next sequence number to be transmitted */
    uint32_t next_tx_sequence_number;
    /* The next index into tested_port_indices[] for the next destination port to use for a transmitted frame */
    uint32_t destination_tested_port_index;
    /* The next modulo offset from destination_tested_port_index to use as the source port for a transmitted frame */
    uint32_t source_port_offset;
    /* Contains the pending receive frames, indexed by [source_port][destination_port] */
    pending_rx_frames_t pending_rx_frames[NUM_DEFINED_PORTS][NUM_DEFINED_PORTS];
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
     *   9 ports are tested then need to rate limit the transmission.
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
 * The semaphores control the access by:
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


/*
 * @brief Exit after a fatal error, cleaning up the EAL.
 */
static void exit_cleaning_up_eal (void)
{
    (void) rte_eal_cleanup();
    exit (EXIT_FAILURE);
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

    if (console_file != NULL)
    {
        va_start (args, format);
        vfprintf (console_file, format, args);
        va_end (args);
    }

    va_start (args, format);
    vprintf (format, args);
    va_end (args);
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

        if (console_file != NULL)
        {
            va_start (args, format);
            fprintf (console_file, "Assertion failed : ");
            vfprintf (console_file, format, args);
            va_end (args);
            fprintf (console_file, "\n");
        }

        va_start (args, format);
        fprintf (stderr, "Assertion failed : ");
        vfprintf (stderr, format, args);
        va_end (args);
        fprintf (stderr, "\n");
        exit_cleaning_up_eal ();
    }
}


/**
 * @brief Display the application usage, in terms of non-EAL options used by the application
 * @details May be called either from EAL in response to -help, or from the application when
 *          invalid application arguments are used.
 * @param[in] program_name Name of the program.
 */
static void display_application_usage (const char *const program_name)
{
    printf ("%s Application options:\n", program_name);
    printf ("  [-t <duration_secs>] [-d] [-p <port_list>] [-r <rate_mbps>]\n");
    printf ("\n");
    printf ("  -d enables debug mode, where runs just for a single test interval and creates\n");
    printf ("     a CSV file containing the frames sent/received.\n");
    printf ("\n");
    printf ("  -t defines the duration of a test interval in seconds, over which the number\n");
    printf ("     errors is accumulated and reported.\n");
    printf ("  -p defines which switch ports to test. The <port_list> is a comma separated\n");
    printf ("     string, with each item either a single port number or a start and end port\n");
    printf ("     range delimited by a dash. E.g. 1,4-6 specifies ports 1,4,5,6.\n");
    printf ("     If not specified defaults to all %u defined ports\n", NUM_DEFINED_PORTS);
    printf ("  -r Specifies the bit rate generated on each port on the switch under test,\n");
    printf ("     as a floating point mega bits per second. Default is %g\n", DEFAULT_TESTED_PORT_MBPS);
}


/**
 * @brief Display application usage and then exit the process
 * @param[in] program_name Name of the program.
 */
static void display_usage (const char *const program_name)
{
    display_application_usage (program_name);
    exit_cleaning_up_eal ();
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
        exit_cleaning_up_eal ();
    }

    /* Check the port number hasn't been specified more than once */
    for (uint32_t tested_port_index = 0; tested_port_index < num_tested_port_indices; tested_port_index++)
    {
        if (test_ports[tested_port_indices[tested_port_index]].switch_port_number == port_num)
        {
            printf ("Error: Switch port %" PRIu32 " specified more than once\n", port_num);
            exit_cleaning_up_eal ();
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
            exit_cleaning_up_eal ();
        }
        range_start_port_num = port_num;

        port_range_token = strtok_r (NULL, range_delim, &port_range_saveptr);
        if (port_range_token != NULL)
        {
            /* A range of port numbers specified */
            if (sscanf (port_range_token, "%" SCNu32 "%c", &port_num, &junk) != 1)
            {
                printf ("Error: %s is not a valid port number\n", port_range_token);
                exit_cleaning_up_eal ();
            }
            range_end_port_num = port_num;

            port_range_token = strtok_r (NULL, range_delim, &port_range_saveptr);
            if (port_range_token != NULL)
            {
                printf ("Error: %s unexpected spurious port range\n", port_range_token);
                exit_cleaning_up_eal ();
            }

            if (range_end_port_num < range_start_port_num)
            {
                printf ("Error: port range end %" PRIu32 " is less than port range start %" PRIu32 "\n",
                        range_end_port_num, range_start_port_num);
                exit_cleaning_up_eal ();
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
 * @brief Read the application command line arguments, exiting if an error in the arguments.
 * @param[in] argc, argv Application line arguments, from which the EAL parsed options have been removed
 * @param[in] num_eal_parsed_arguments
 */
static void read_command_line_arguments (const int argc, char *argv[])
{
    const char *const program_name = argv[0];
    const char *const optstring = "dt:p:r:";
    int option;
    char junk;

    /* Validate EAL options are correct for this program */
    const int lcore_count = rte_lcore_count ();
    const uint16_t available_eth_dev_count = rte_eth_dev_count_avail ();
    if (lcore_count != 2)
    {
        printf ("Error: the EAL options must specify two lcores (main and one worker), actual number is %d\n", lcore_count);
        exit_cleaning_up_eal ();
    }

    if (available_eth_dev_count != 1)
    {
        printf ("Error: the EAL options must specify only one available Ethernet device, actual number is %" PRIu16 "\n",
                available_eth_dev_count);
        exit_cleaning_up_eal ();
    }

    /* Default to testing all defined switch ports */
    num_tested_port_indices = 0;
    for (uint32_t port_index = 0; port_index < NUM_DEFINED_PORTS; port_index++)
    {
        tested_port_indices[num_tested_port_indices] = port_index;
        num_tested_port_indices++;
    }

    /* Process the command line arguments */
    option = getopt (argc, argv, optstring);
    while (option != -1)
    {
        switch (option)
        {

        case 'd':
            arg_frame_debug_enabled = true;
            break;

        case 't':
            if ((sscanf (optarg, "%" SCNi64 "%c", &arg_test_interval_secs, &junk) != 1) ||
                (arg_test_interval_secs <= 0))
            {
                printf ("Error: Invalid <duration_secs> %s\n", optarg);
                exit_cleaning_up_eal ();
            }
            break;

        case 'p':
            parse_tested_port_list (optarg);
            break;

        case 'r':
            if (sscanf (optarg, "%lf%c", &arg_tested_port_mbps, &junk) != 1)
            {
                printf ("Error: Invalid <rate_mbps> %s\n", optarg);
                exit_cleaning_up_eal ();
            }
            break;

        default:
            display_usage (program_name);
            break;
        }

        option = getopt (argc, argv, optstring);
    }

    /* Check the expected arguments have been provided */
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
 * @brief Open the DPDK device used to send/receive test frames
 * @param[in/out] context The context being initialised.
 */
static void open_dpdk_device (frame_tx_rx_thread_context_t *const context)
{
    int rc;
    struct rte_eth_conf port_conf;
    struct rte_eth_txq_info tx_qinfo;

    /* Find the identity of the worker lcore_id used by the test */
    context->worker_lcore_id = rte_get_next_lcore (-1, 1, 0);
    check_assert (context->worker_lcore_id != RTE_MAX_LCORE, "failed to determine worker lcore_id");

    /* Get the identity of the first available port to be used for the test.
     * read_command_line_arguments() has already verified that the command line arguments have selected
     * a single available port. */
    context->port_id = rte_eth_find_next_owned_by (0, RTE_ETH_DEV_NO_OWNER);
    CHECK_ASSERT (context->port_id < RTE_MAX_ETHPORTS);
    CHECK_ASSERT (rte_eth_dev_is_valid_port (context->port_id));

    /* Get the device information for information about descriptor limits */
    rc = rte_eth_dev_info_get (context->port_id, &context->dev_info);
    CHECK_ASSERT (rc == 0);
    console_printf ("Using DPDK driver_name=%s device_name=%s\n",
            context->dev_info.driver_name, context->dev_info.device->name);

    /* Warn if the worker lcore and Ethernet device are on different sockets (NUMA nodes) as may impact performance */
    const int lcore_socket_id = rte_lcore_to_socket_id (context->worker_lcore_id);
    const int device_socket_id = rte_eth_dev_socket_id (context->port_id);
    if (lcore_socket_id != device_socket_id)
    {
        console_printf ("Warning: worked_lcore_id %d is on a different socket_id %d to the device on socket_id %d (may impact performance)\n",
                context->worker_lcore_id, lcore_socket_id, device_socket_id);
    }

    /* Configure device using defaults, and no offloads enabled */
    memset (&port_conf, 0, sizeof (port_conf));
    rc = rte_eth_dev_configure (context->port_id, NUM_RX_QUEUES, NUM_TX_QUEUES, &port_conf);

    /* Limit the burst which can be queued for transmission to the number of ports on the switch under test,
     * to avoid potentially overloading switch ports if the software gets behind. */
    const uint16_t desired_tx_num_descriptors = (uint16_t) num_tested_port_indices;

    /* @todo Is this an over estimate? While smaller may make better use of cache not sure how that works
     *       with DPDK (which has to target memory?).
     *       Also, the comment about the rationale for the value of NOMINAL_TOTAL_PENDING_RX_FRAMES has
     *       been copied from the code which used pcap.
     *
     *       With pcap the amount of transmit and receive buffering was not visible to the program.
     *
     *       Whereas with DPDK this program can limit the maximum number of Tx frames which can be queued
     *       and therefore should be determine the maximum number of pending Rx frames after some allowance
     *       for flooded packets / other packets.
     *
     *       However, can the switches delay sending frames? */
    const uint16_t desired_rx_num_descriptors = NOMINAL_TOTAL_PENDING_RX_FRAMES;

    /* Adjust the number of descriptors to any device imposed limits */
    context->tx_num_descriptors = desired_tx_num_descriptors;
    context->rx_num_descriptors = desired_rx_num_descriptors;
    rc = rte_eth_dev_adjust_nb_rx_tx_desc (context->port_id, &context->rx_num_descriptors, &context->tx_num_descriptors);
    CHECK_ASSERT (rc == 0);

    /* Allocate and create the transmit queue, using a default configuration */
    rc = rte_eth_tx_queue_setup (context->port_id, QUEUE_ID, context->tx_num_descriptors, device_socket_id, NULL);
    CHECK_ASSERT (rc == 0);

    /* As of DPDK 21.11.0 the mlx5_pci driver can increase the number of transmit descriptors when
     * rte_eth_tx_queue_info_get() to more than that set by rte_eth_dev_adjust_nb_rx_tx_desc().
     * Attempt to get the information of the created transmit queue, and check if the number of descriptors
     * has changed.
     *
     * Some drivers, e.g. net_qede, don't support rte_eth_tx_queue_info_get which is the reason for the test
     * on ENOTSUP. */
    rc = rte_eth_tx_queue_info_get (context->port_id, QUEUE_ID, &tx_qinfo);
    if (rc != -ENOTSUP)
    {
        CHECK_ASSERT (rc == 0);
        if (tx_qinfo.nb_desc != context->tx_num_descriptors)
        {
            console_printf ("rte_eth_tx_queue_info_get() changed number of Tx descriptors from %" PRIu16 " to %" PRIu16 "\n",
                    context->tx_num_descriptors, tx_qinfo.nb_desc);
            context->tx_num_descriptors = tx_qinfo.nb_desc;
        }
    }

    /* Handle drivers which needs the number of ring descriptors as a power-of-two, but for which
     * rte_eth_dev_adjust_nb_rx_tx_desc() doesn't apply the adjustment. */
    if ((strcmp (context->dev_info.driver_name, "net_qede") == 0) ||
        (strcmp (context->dev_info.driver_name, "mlx5_pci") == 0))
    {
        context->rx_num_descriptors = rte_align32pow2 (context->rx_num_descriptors);
        context->tx_num_descriptors = rte_align32pow2 (context->tx_num_descriptors);
    }

    /* Create the pool used for transmit. The number of elements in the pool is one more than the number of
     * descriptors so can still obtain one more mbuf if the transmit ring becomes full. The act of trying to
     * queue a mbuf for transmission is the means by which the DPDK PMD will check for a free descriptor.
     * Default buffer size is sufficient since are using the maximum length standard Ethernet frame.
     * I.e. not using jumbo frames.
     * No cache is used since only used by a single lcore.
     * No private data is needed. */
    context->tx_mbuf_pool = rte_pktmbuf_pool_create ("MBUF_POOL_TX", context->tx_num_descriptors + 1,
            0, 0, RTE_MBUF_DEFAULT_BUF_SIZE, lcore_socket_id);
    CHECK_ASSERT (context->tx_mbuf_pool != NULL);

    /* Create the pool used for receive. Default buffer size is sufficient since are using the maximum length
     * standard Ethernet frame. I.e. not using jumbo frames.
     * No cache is used since only used by a single lcore.
     * No private data is needed. */
    context->rx_mbuf_pool = rte_pktmbuf_pool_create ("MBUF_POOL_RX", context->rx_num_descriptors,
            0, 0, RTE_MBUF_DEFAULT_BUF_SIZE, lcore_socket_id);
    CHECK_ASSERT (context->rx_mbuf_pool != NULL);

    context->rx_pkts = calloc (context->rx_num_descriptors, sizeof (context->rx_pkts[0]));
    CHECK_ASSERT (context->rx_pkts != NULL);

    /* Allocate and create the receive queue, using a default configuration */
    rc = rte_eth_rx_queue_setup (context->port_id, QUEUE_ID, context->rx_num_descriptors,
            device_socket_id, NULL, context->rx_mbuf_pool);
    CHECK_ASSERT (rc == 0);

    /* Start the Ethernet device */
    rc = rte_eth_dev_start (context->port_id);
    CHECK_ASSERT (rc == 0);

    /* Display the desired number of descriptors, and the actual number to meet the limits of the device */
    console_printf ("tx_num_descriptors : desired=%" PRIu16 " actual=%" PRIu16 "\n",
            desired_tx_num_descriptors, context->tx_num_descriptors);
    console_printf ("rx_num_descriptors : desired=%" PRIu16 " actual=%" PRIu16 "\n",
            desired_rx_num_descriptors, context->rx_num_descriptors);
}


/**
 * @brief Get the of the DPDK Ethernet device being used for the test
 * @details Waits until has got a valid link speed, since for Network devices using DPDK-compatible driver
 *          the link may not start to come up until rte_eth_dev_start() has been called
 * @return The rate in Mbps, or RTE_ETH_SPEED_NUM_UNKNOWN if the user used Ctrl-C to abort waiting
 *         to obtain the link speed.
 */
static uint32_t get_rate_mbps (const frame_tx_rx_thread_context_t *const context)
{
    int rc;
    struct rte_eth_link link;
    bool link_speed_valid;
    const struct timespec hold_off =
    {
        .tv_sec = 0,
        .tv_nsec = 100000000 /* 100 milliseconds */
    };
    bool first_wait = true;

    do
    {
        rc = rte_eth_link_get_nowait (context->port_id, &link);
        CHECK_ASSERT (rc == 0);
        link_speed_valid = (link.link_status == RTE_ETH_LINK_UP) &&
                (link.link_speed != RTE_ETH_SPEED_NUM_NONE) && (link.link_speed != RTE_ETH_SPEED_NUM_UNKNOWN);

        if (!link_speed_valid)
        {
            if (first_wait)
            {
                console_printf ("Waiting to get link speed (link needs to be up). Press Ctrl-C to abort.\n");
                first_wait = false;
            }
            clock_nanosleep (CLOCK_MONOTONIC, 0, &hold_off, NULL);
        }
    } while (!link_speed_valid && !test_stop_requested);

    return link_speed_valid ? link.link_speed : RTE_ETH_SPEED_NUM_UNKNOWN;
}


int main (int argc, char *argv[])
{
    int num_eal_parsed_arguments;
    int rc;

    /* Check that ethercat_frame_t has the expected size */
    if (sizeof (ethercat_frame_t) != 1518)
    {
        fprintf (stderr, "sizeof (ethercat_frame_t) unexpected value of %" PRIuPTR, sizeof (ethercat_frame_t));
        exit (EXIT_FAILURE);
    }

    /* Initialise the semaphores used to control access to the test interval statistics */
    rc = sem_init (&test_statistics_free, 0, 1);
    CHECK_ASSERT (rc == 0);
    rc = sem_init (&test_statistics_populated, 0, 0);
    CHECK_ASSERT (rc == 0);

    /* Initialise the DPDK EAL */
    (void) rte_set_application_usage_hook (display_application_usage);
    num_eal_parsed_arguments = rte_eal_init (argc, argv);
    if (num_eal_parsed_arguments < 0)
    {
        printf ("rte_eal_init() failed : %s\n", strerror (-num_eal_parsed_arguments));
        exit_cleaning_up_eal ();
    }

    /* Parse remaining arguments which our for out application, ignoring those parsed by the DPDK EAL */
    read_command_line_arguments (argc - num_eal_parsed_arguments, &argv[num_eal_parsed_arguments]);

    /* Try and avoid page faults while running */
    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    if (rc != 0)
    {
        printf ("mlockall() failed\n");
    }

    /* Set filenames which contain the output files containing the date/time and OS used  */
    results_summary_t results_summary = {{0}};
    const time_t tod_now = time (NULL);
    struct tm broken_down_time;
    char date_time_str[80];
    char frame_debug_csv_filename[160];
    char console_filename[160];

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

    /* Open the DPDK device selected by the command line */
    frame_tx_rx_thread_context_t *const tx_rx_thread_context = calloc (1, sizeof (*tx_rx_thread_context));
    open_dpdk_device (tx_rx_thread_context);

    /* Install signal handler, used to request test is stopped */
    struct sigaction action;

    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_test_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    CHECK_ASSERT (rc == 0);

    /* Get the bit rate of the Ethernet link used by this program */
    const uint32_t injection_port_bit_rate_mbps = get_rate_mbps (tx_rx_thread_context);

    if (injection_port_bit_rate_mbps != RTE_ETH_SPEED_NUM_UNKNOWN)
    {
        const int64_t injection_port_bit_rate = 1000000L * (int64_t) injection_port_bit_rate_mbps;
        console_printf ("Bit rate on interface to injection switch = %d (Mbps)\n", injection_port_bit_rate_mbps);

        /* The requested bit rate to be generated across all the switch ports under test */
        const int64_t requested_switch_under_test_bit_rate = lround (arg_tested_port_mbps * 1E6) * num_tested_port_indices;
        console_printf ("Requested bit rate to be generated on each switch port under test = %.2f (Mbps)\n", arg_tested_port_mbps);

        /* Decide if need to limit the transmitted frame rate or not */
        if (injection_port_bit_rate > requested_switch_under_test_bit_rate)
        {
            const double limited_frame_rate = (double) requested_switch_under_test_bit_rate / (double) TEST_PACKET_BITS;
            tx_rx_thread_context->tx_interval = lround (1E9 / limited_frame_rate);
            tx_rx_thread_context->tx_rate_limited = true;
            console_printf ("Limiting max frame rate to %.1f Hz, as bit-rate on interface to injection switch exceeds that across all switch ports under test\n",
                    limited_frame_rate);
        }
        else
        {
            tx_rx_thread_context->tx_rate_limited = false;
            console_printf ("Not limiting frame rate, as bit-rate on interface to injection switch doesn't exceed the total across all switch ports under test\n");
        }

        /* Report the command line arguments used */
        console_printf ("Writing per-port counts to %s\n", results_summary.per_port_counts_csv_filename);
        console_printf ("Test interval = %" PRIi64 " (secs)\n", arg_test_interval_secs);
        console_printf ("Frame debug enabled = %s\n", arg_frame_debug_enabled ? "Yes" : "No");
    }
    else
    {
        console_printf ("Test aborted while waiting to obtain the link speed\n");
    }

    /* Stop and close the Ethernet device */
    rc = rte_eth_dev_stop (tx_rx_thread_context->port_id);
    CHECK_ASSERT (rc == 0);
    rc = rte_eth_dev_close (tx_rx_thread_context->port_id);
    CHECK_ASSERT (rc == 0);

    /* clean up the EAL */
    rc = rte_eal_cleanup();
    if (rc != 0)
    {
        printf ("rte_eal_cleanup() failed\n");
        exit (EXIT_FAILURE);
    }

    return EXIT_SUCCESS;
}
