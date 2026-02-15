/*
 * @file ibv_generate_infiniband_test_load.c
 * @date 2 Jun 2019
 * @author Chester Gillon
 * @details
 *   Program to generate an RDMA test load, for the purpose of generating maximum transfer rate across all
 *   Infiniband (or RoCE) ports on a host with minimum CPU load.
 *
 *   To minimise the CPU load the test queues RDMA write transfers of 256MB size, and blocks waiting for a transfer to
 *   complete before queueing another transfer. Each pair of Infiniband ports tests can have two transfers queued, so while
 *   the software is re-queueing a complete transfer the other transfer should be in progress such that the Infiniband DMA
 *   is constantly busy.
 *
 *   The reason to minimise the CPU load is to allow other programs such as FIRESTARTER to be producing a maximum power
 *   consumption on all CPU cores.
 *
 *   This program assumes there are one or more pairs of Infiniband protocol capable HCA ports, and for each pair of ports attempts
 *   a full duplex transfer from port 1 -> 2 as well as port 2 -> 1. By effectively looping back the pair of ports
 *   via a connected switch the program can be run a single instance on a host to exercise all local Infiniband ports independent
 *   of any other hosts.
 *
 *   E.g. if run on a host with two dual-port Infiniband HCAs then will queue transmission as fast as possible for the following
 *   combinations:
 *   a. mlx4_0 port 1 tx -> mlx4_0 port 2 rx
 *   b. mlx4_0 port 2 tx -> mlx4_0 port 1 rx
 *   c. mlx4_1 port 1 tx -> mlx4_1 port 2 rx
 *   d. mlx4_1 port 2 tx -> mlx4_1 port 1 rx
 *
 *   The transmit and receive memory buffers are bound to a specific NUMA node, with a command line offset applied between the
 *   NUMA node local to the the Infiniband HCA and that used for the memory buffers.
 *   E.g. in a dual processor system with an Infiniband HCA connected to each processor then:
 *   a. With a numa_node_offset of zero then:
 *      - The mlx4_0 HCA connected to the processor for NUMA node 0 will access memory on NUMA node 0
 *      - The mlx4_1 HCA connected to the processor for NUMA node 1 will access memory on NUMA node 1
 *
 *   b. With a numa_node_offset of one then:
 *      - The mlx4_0 HCA connected to the processor for NUMA node 0 will access memory on NUMA node 1
 *      - The mlx4_1 HCA connected to the processor for NUMA node 1 will access memory on NUMA node 0
 *
 *      In this configuration QPI traffic will be generated to access the memory buffers.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>
#include <errno.h>

#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <poll.h>
#include <numa.h>
#include <limits.h>
#include <linux/mempolicy.h>
#include <numaif.h>

#include <infiniband/verbs.h>


/* Defines the size and number of buffers used to transmit test data on each port.
 * Uses large individual buffers to minimise the CPU overhead of keeping the RDMA busy.
 * Total size per port sufficient for a PRBS32 pattern. */
#define TEST_BUFFER_SIZE_BYTES 0x10000000
#define TEST_BUFFER_SIZE_WORDS (TEST_BUFFER_SIZE_BYTES / sizeof (uint32_t))
#define NUM_TEST_BUFFERS_PER_PORT 2


/* Used to size arrays for the number of RDMA connections used in the test, where each connection is a simplex transfer
 * from source to destination. */
#define MAX_RDMA_TEST_CONNECTIONS 8

/* Used to build a wr_id which identifies which pair / port / buffer a RDMA write work-request is for.
 * These as used so that a work-request completed can determine which request to re-queue */
#define WR_ID_CONNECTION_INDEX_SHIFT   1
#define WR_ID_CONNECTION_INDEX_MASK  0x7
#define WR_ID_BUFFER_INDEX_SHIFT       0
#define WR_ID_BUFFER_INDEX_MASK      0x1


/* Used to allocate space for the transmit or receive buffers used for one end of a RDMA connection */
typedef struct
{
    uint32_t buffers[NUM_TEST_BUFFERS_PER_PORT][TEST_BUFFER_SIZE_WORDS];
} test_connection_buffers_t;


/* Used to obtain the RDMA port counts from one of the ports under test */
typedef struct
{
    /* From IB_PC_EXT_XMT_BYTES_F, which is actual a count of 32-bit words */
    uint64_t tx_words;
    /* From IB_PC_EXT_RCV_BYTES_F, which is actual a count of 32-bit words */
    uint64_t rx_words;
} rdma_port_counters_t;


/* Defines the context for one RDMA device on the local host, which may be used for the test.
 * The device content may be shared by multiple connections.
 *
 * All RDMA devices on the host are opened, even if they end up not being used by the test, in order to
 * determine which RDMA devices can be used. */
typedef struct
{
    /* The RDMA device */
    struct ibv_device *device;
    /* The opened RDMA device context */
    struct ibv_context *context;
    /* The RDMA device attribute */
    struct ibv_device_attr device_attributes;
    /* When true the device is used by the test, and the remaining fields are valid */
    bool used;
    /* The protection domain for the device */
    struct ibv_pd *device_pd;
    /* The completion queue used for RDMA writes on the port, so the main thread can be used to block waiting for
     * completion. */
    struct ibv_cq *cq;
    /* The completion channel used to wait for completions in cq */
    struct ibv_comp_channel *completion_channel;
    /* The local NUMA node for the device */
    int numa_node;
    /* Which NUMA node used for buffer allocation for this device. Is NULL if no NUMA nodes on the host. */
    struct bitmask *buffers_numa_mask;
} rdma_test_device_t;


/* The context for the connection between two RDMA ports used for the test,
 * used to perform RDMA writes from the source to destination port. */
typedef struct
{
    /* The RDMA source and destination devices */
    rdma_test_device_t *source_device;
    rdma_test_device_t *destination_device;
    /* The source and destination ports used for this context */
    uint8_t source_port;
    uint8_t destination_port;
    /* The source and destination GIDs when using Ethernet as the link layer (i.e. RoCE) */
    struct ibv_gid_entry source_gid_entry;
    struct ibv_gid_entry destination_gid_entry;
    /* The zero-based index of this RDMA device used when setting the wr_id for the transfers */
    uint32_t device_index;
    /* The allocated buffers this connection. Tx and Rx buffers are different allocations as may be for devices on different
     * NUMA nodes. */
    test_connection_buffers_t *tx_buffers;
    test_connection_buffers_t *rx_buffers;
    /* The RDMA memory regions to access the buffers */
    struct ibv_mr *tx_buffers_mr;
    struct ibv_mr *rx_buffers_mr;
    /* The attributes of the ports used for the transfers */
    struct ibv_port_attr tx_port_attributes;
    struct ibv_port_attr rx_port_attributes;
    /* The queue-pairs used for the transfers. The rx_qp is only used during initialisation, since use RDMA writes. */
    struct ibv_qp *tx_qp;
    struct ibv_qp *rx_qp;
    /* The RDMA write transfers for each buffer */
    struct ibv_send_wr wrs[NUM_TEST_BUFFERS_PER_PORT];
    struct ibv_sge sges[NUM_TEST_BUFFERS_PER_PORT];
    /* The total number of transfers which have completed for this context */
    uint32_t num_completed_transfers;
    /* The CLOCK_MONOTONIC time of the most recent transfer completion */
    struct timespec last_completion_time;
    /* The RDMA counters for the source_port at the start and end of the test, for correlating the amount of traffic
     * on the RDMA ports against the total transferred by RDMA writes. */
    rdma_port_counters_t source_port_counters_start;
    rdma_port_counters_t source_port_counters_end;
} test_load_connection_context_t;


/* The context for the test */
typedef struct
{
    /* The number of RDMA devices present, not all of which may be used by the test */
    uint32_t num_rdma_devices;
    /* The list of RDMA devices */
    struct ibv_device **rdma_device_list;
    /* Test specific context for each RDMA device present.
     * Which fields are populated depends upon if a device is used by the test. */
    rdma_test_device_t devices[MAX_RDMA_TEST_CONNECTIONS];
    /* The number of RDMA connections used by the test */
    uint32_t num_connections;
    /* The RDMA connections used by the test.
     * Valid indices are in the range [0..num_connections-1]. */
    test_load_connection_context_t connections[MAX_RDMA_TEST_CONNECTIONS];
    /* The number of file descriptors use to wait for completion, which is the number of RDMA devices used */
    uint32_t num_poll_fds;
    /* File descriptors used to block waiting for completion from any device.
     * Valid indices are in the range [0..num_poll_fds-1]. */
    struct pollfd poll_fds[MAX_RDMA_TEST_CONNECTIONS];
    /* For each poll_fds[] element, gives the index into devices[] for the corresponding RDMA device */
    uint32_t poll_fds_device_indicies[MAX_RDMA_TEST_CONNECTIONS];
    /* The CLOCK_MONOTONIC time at which started to queue the test transfers */
    struct timespec test_start_time;
} test_load_context_t;


/* Starting valid for PRBS32 test pattern. Any non-zero value sufficient */
#define PRBS32_PATTERN_START 1u


/** Set from a signal handler to request that the transmission of the test load stops */
static volatile bool stop_transmission;


/* Command line argument which defines the offset between the RDMA device NUMA node and the NUMA node in which the
 * RDMA buffers are placed. */
static int arg_numa_node_offset;


/* Command line argument which defines which GID index is used for an Ethernet link layer.
 * Used to allow selection of the RoCE protocol version. */
static uint8_t arg_ethernet_gid_index = 0;


/* Names displayed for ibv_gid_type */
static const char *const gid_type_names[] =
{
    [IBV_GID_TYPE_IB     ] = "IB",
    [IBV_GID_TYPE_ROCE_V1] = "RoCE V1",
    [IBV_GID_TYPE_ROCE_V2] = "RoCE V2"

};


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
 * @brief Signal handler to request transmission of test transfers is stopped
 */
static void stop_transmission_handler (const int sig)
{
    stop_transmission = true;
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
 * @brief Initialise a tx_buffer with a PRBS32 (pseudorandom binary sequence) test pattern
 * @param[out] tx_buffer The transmit buffer to initialise with the test pattern.
 */
static void initialise_tx_pattern_buffer (uint32_t tx_buffer[const TEST_BUFFER_SIZE_WORDS])
{
    static bool pattern_complete = false;
    static uint32_t lfsr = PRBS32_PATTERN_START;
    static uint64_t period = 0;
    uint32_t buffer_index;
    uint32_t bit_index;
    uint32_t bit;

    for (buffer_index = 0; buffer_index < TEST_BUFFER_SIZE_WORDS; buffer_index++)
    {
        for (bit_index = 0; bit_index < 32; bit_index++)
        {
            /* taps: 32 22 2 1; feedback polynomial: x^32 + x^22 + x^2 + x^1 + 1 */
            bit = ((lfsr >> 0) ^ (lfsr >> 10) ^ (lfsr >> 30) ^ (lfsr >> 31)) /* & 1u */;
            lfsr = (lfsr >> 1) | (bit << 31);
            period++;
            if ((lfsr == PRBS32_PATTERN_START) && (!pattern_complete))
            {
                /* Display the period of the PRBS32 pattern as a cross check that is correct */
                pattern_complete = true;
                printf ("PRBS32 pattern period is %" PRIu64 "\n", period);
            }
        }
        tx_buffer[buffer_index] = lfsr;
    }
}


/*
 * @brief Get the local RDMA devices on the host
 * @details This is done before parse command line options so that can validate requested RDMA devices for connections are present.
 * @param[out] context The initialised context with just the list of local RDMA devices
 */
static void get_local_rdma_devices (test_load_context_t *const context)
{
    int rc;
    int num_rdma_devices = 0;

    memset (context, 0, sizeof (*context));

    context->num_connections = 0;
    context->num_poll_fds = 0;
    context->rdma_device_list = ibv_get_device_list (&num_rdma_devices);
    context->num_rdma_devices = (uint32_t) num_rdma_devices;
    for (int device_index = 0; device_index < context->num_rdma_devices; device_index++)
    {
        rdma_test_device_t *const device = &context->devices[device_index];

        device->used = false;
        device->device = context->rdma_device_list[device_index];
        device->context = ibv_open_device (device->device);
        CHECK_ASSERT (device->device != NULL);
        rc = ibv_query_device (device->context, &device->device_attributes);
        CHECK_ASSERT (rc == 0);
    }
}


/**
 * @brief Get one RDMA test device, performing the per-device initialisation on first use.
 * @param[in/out] context The test context being initialised.
 * @param[in] device_index Which RDMA device to get
 * @return The initialised RDMA device
 */
static rdma_test_device_t *get_rdma_test_device (test_load_context_t *const context, const uint32_t device_index)
{
    CHECK_ASSERT (device_index < context->num_rdma_devices);
    rdma_test_device_t *const test_device = &context->devices[device_index];
    char sysfs_path[PATH_MAX];
    FILE *sysfs_file;
    int buffers_numa_node;

    if (!test_device->used)
    {
        /* Allocate protection domain */
        test_device->device_pd = ibv_alloc_pd (test_device->context);
        CHECK_ASSERT (test_device->device_pd != NULL);

        /* Create the completion queue using a completion channel which will be used to block waiting for completion */
        test_device->completion_channel = ibv_create_comp_channel (test_device->context);
        CHECK_ASSERT (test_device->completion_channel != NULL);
        context->poll_fds[context->num_poll_fds].fd = test_device->completion_channel->fd;
        context->poll_fds[context->num_poll_fds].events = POLLIN;
        context->poll_fds_device_indicies[context->num_poll_fds] = device_index;
        test_device->cq = ibv_create_cq (test_device->context, MAX_RDMA_TEST_CONNECTIONS * NUM_TEST_BUFFERS_PER_PORT, NULL,
                test_device->completion_channel, 0);
        CHECK_ASSERT (test_device->cq != NULL);

        /* Find the local NUMA for the RDMA device */
        snprintf (sysfs_path, sizeof (sysfs_path), "%s/device/numa_node", test_device->device->ibdev_path);
        sysfs_file = fopen (sysfs_path, "r");
        const int num_items = fscanf (sysfs_file, "%d", &test_device->numa_node);
        CHECK_ASSERT (num_items == 1);
        fclose (sysfs_file);

        test_device->buffers_numa_mask = NULL;
        if (test_device->numa_node != -1)
        {
            test_device->buffers_numa_mask = numa_allocate_nodemask ();
            CHECK_ASSERT (test_device->buffers_numa_mask != NULL);
            buffers_numa_node = (test_device->numa_node + arg_numa_node_offset) % numa_num_configured_nodes ();
            numa_bitmask_setbit (test_device->buffers_numa_mask, buffers_numa_node);
        }

        context->num_poll_fds++;
        test_device->used = true;
    }

    return test_device;
}


/**
 * @brief Allocate the transmit or receive buffers for a test connection
 * @param test_device[in/out] The RDMA device the buffers will be accessed by
 * @param buffers[out] The allocated buffers in the virtual address space of the process
 * @param buffer_mr[out] Used by the RDMA device to access the buffers
 */
static void allocate_connection_buffers (rdma_test_device_t *const test_device,
                                         test_connection_buffers_t **const buffers, struct ibv_mr **const buffers_mr)
{
    int rc;
    long lrc;
    const int pagesize = getpagesize ();

    void *buffers_alloc = NULL;
    rc = posix_memalign (&buffers_alloc, pagesize, sizeof (test_connection_buffers_t));
    CHECK_ASSERT (rc == 0);
    *buffers = buffers_alloc;

    if (test_device->buffers_numa_mask != NULL)
    {
        /* When the computer has multiple NUMA nodes, bind the buffers to a NUMA node
         * with a relative offset to that of the NUMA node local to the RDMA device. */
        lrc = mbind (*buffers, sizeof (test_connection_buffers_t), MPOL_F_STATIC_NODES | MPOL_BIND,
                test_device->buffers_numa_mask->maskp, test_device->buffers_numa_mask->size, MPOL_MF_STRICT | MPOL_MF_MOVE);
        CHECK_ASSERT (lrc == 0);
    }

    *buffers_mr = ibv_reg_mr (test_device->device_pd, *buffers, sizeof (test_connection_buffers_t),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    CHECK_ASSERT (*buffers_mr != NULL);
}


/**
 * @brief Initialise the transfers for one RDMA connection between a pair ports to generate a test load
 * @details Since the source and destination ports are in the same PC and accessed by this program, the queue-pairs can be
 *          initialised with a simple sequence without requiring any out-of-band communication of the queue-pair attributes.
 * @param context[in/out] The test context to add the connection to
 * @param source_device_index[in] The index of the source RDMA device used for the connection
 * @param source_port[in] The port on the source device to use for transmit
 * @param destination_device_index[in] The index of the destination RDMA device used for the connection
 * @param destination_port[in] The port on the destination device to use for receive
 */
static void initialise_connection_transfers (test_load_context_t *const context,
                                             const uint32_t source_device_index, const uint8_t source_port,
                                             const uint32_t destination_device_index, const uint8_t destination_port)
{
    const uint32_t tx_psn = get_random_psn ();
    const uint32_t rx_psn = get_random_psn ();
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;
    uint32_t buffer_index;

    /* Default values */
    const uint8_t timeout = 16;
    const uint8_t retry_cnt = 7;

    if (context->num_connections == MAX_RDMA_TEST_CONNECTIONS)
    {
        printf ("Compile time %u maximum number of connections exceeded\n", MAX_RDMA_TEST_CONNECTIONS);
        exit (EXIT_FAILURE);
    }
    const uint32_t connection_index = context->num_connections;
    test_load_connection_context_t *const connection = &context->connections[connection_index];

    connection->source_device =  get_rdma_test_device (context, source_device_index);
    connection->destination_device = get_rdma_test_device (context, destination_device_index);
    connection->source_port = source_port;
    connection->destination_port = destination_port;
    rc = ibv_query_port (connection->source_device->context, source_port, &connection->tx_port_attributes);
    CHECK_ASSERT (rc == 0);
    rc = ibv_query_port (connection->destination_device->context, destination_port, &connection->rx_port_attributes);
    CHECK_ASSERT (rc == 0);

    /* Create queue-pairs */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = connection->source_device->cq;
    qp_init_attr.recv_cq = connection->destination_device->cq; /* Receive completion queue not used */
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = NUM_TEST_BUFFERS_PER_PORT;
    qp_init_attr.cap.max_send_sge = NUM_TEST_BUFFERS_PER_PORT;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = true;
    connection->tx_qp = ibv_create_qp (connection->source_device->device_pd, &qp_init_attr);
    CHECK_ASSERT (connection->tx_qp != NULL);

    qp_init_attr.cap.max_send_wr = 0;
    qp_init_attr.cap.max_send_sge = 0;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    connection->rx_qp = ibv_create_qp (connection->destination_device->device_pd, &qp_init_attr);
    CHECK_ASSERT (connection->rx_qp != NULL);

    /* Transition queue-pairs to the INIT state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = source_port;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (connection->tx_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    CHECK_ASSERT (rc == 0);

    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = destination_port;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (connection->rx_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    CHECK_ASSERT (rc == 0);

    /* Transition the queue-pairs to the Ready-to-Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = connection->tx_port_attributes.active_mtu;
    qp_attr.dest_qp_num = connection->rx_qp->qp_num;
    qp_attr.rq_psn = rx_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    if (connection->rx_port_attributes.link_layer == IBV_LINK_LAYER_ETHERNET)
    {
        /* When the link later is Ethernet use the GID index specified by the command line argument.
         * Diagnostic error reported upon failure, since failure may be caused by an out-of-range GID index command line argument. */
        qp_attr.ah_attr.is_global = true;
        qp_attr.ah_attr.grh.sgid_index = arg_ethernet_gid_index;
        rc = ibv_query_gid_ex (connection->destination_device->context, destination_port, qp_attr.ah_attr.grh.sgid_index,
                &connection->destination_gid_entry, 0);
        check_assert (rc == 0, "ibv_query_gid_ex() for %s port %u GID index %u failed with %s\n",
                connection->destination_device->device->name, destination_port, qp_attr.ah_attr.grh.sgid_index, strerror (rc));
        qp_attr.ah_attr.grh.dgid = connection->destination_gid_entry.gid;
        qp_attr.ah_attr.grh.hop_limit = 1;
    }
    else
    {
        /* For Infiniband use LID addressing */
        qp_attr.ah_attr.is_global = false;
        connection->destination_gid_entry.gid_type = IBV_GID_TYPE_IB;
    }
    qp_attr.ah_attr.dlid = connection->rx_port_attributes.lid;
    qp_attr.ah_attr.sl = 0;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = source_port;
    rc = ibv_modify_qp (connection->tx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    if (connection->rx_port_attributes.link_layer == IBV_LINK_LAYER_ETHERNET)
    {
        /* Diagnostic error reported upon failure, since failure may be caused by a GID using an address not on the underlying
         * interface. */
        check_assert (rc == 0, "ibv_modify_qp() IBV_QPS_RTR for %s port %u GID index %u type %s failed with %s\n",
                connection->destination_device->device->name, destination_port, qp_attr.ah_attr.grh.sgid_index,
                gid_type_names[connection->destination_gid_entry.gid_type], strerror (rc));
    }
    CHECK_ASSERT (rc == 0);

    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = connection->rx_port_attributes.active_mtu;
    qp_attr.dest_qp_num = connection->tx_qp->qp_num;
    qp_attr.rq_psn = tx_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    if (connection->tx_port_attributes.link_layer == IBV_LINK_LAYER_ETHERNET)
    {
        /* When the link later is Ethernet use the GID index specified by the command line argument */
        qp_attr.ah_attr.is_global = true;
        qp_attr.ah_attr.grh.sgid_index = arg_ethernet_gid_index;
        rc = ibv_query_gid_ex (connection->source_device->context, source_port, qp_attr.ah_attr.grh.sgid_index,
                &connection->source_gid_entry, 0);
        check_assert (rc == 0, "ibv_query_gid_ex() for %s port %u GID index %u failed with %s\n",
                connection->source_device->device->name, source_port, qp_attr.ah_attr.grh.sgid_index, strerror (rc));
        qp_attr.ah_attr.grh.dgid = connection->source_gid_entry.gid;
        qp_attr.ah_attr.grh.hop_limit = 1;
    }
    else
    {
        /* For Infiniband use LID addressing */
        qp_attr.ah_attr.is_global = false;
        connection->source_gid_entry.gid_type = IBV_GID_TYPE_IB;
    }
    qp_attr.ah_attr.dlid = connection->tx_port_attributes.lid;
    qp_attr.ah_attr.sl = 0;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = destination_port;
    rc = ibv_modify_qp (connection->rx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    if (connection->tx_port_attributes.link_layer == IBV_LINK_LAYER_ETHERNET)
    {
        check_assert (rc == 0, "ibv_modify_qp() IBV_QPS_RTR for %s port %u GID index %u type %s failed with %s\n",
                connection->source_device->device->name, source_port, qp_attr.ah_attr.grh.sgid_index,
                gid_type_names[connection->source_gid_entry.gid_type], strerror (rc));
    }
    CHECK_ASSERT (rc == 0);

    /* Transition the queue-pairs to the Ready-to-Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = tx_psn;
    qp_attr.timeout = timeout;
    qp_attr.retry_cnt = retry_cnt;
    qp_attr.rnr_retry = 0;
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (connection->tx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    CHECK_ASSERT (rc == 0);

    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = rx_psn;
    qp_attr.timeout = timeout;
    qp_attr.retry_cnt = retry_cnt;
    qp_attr.rnr_retry = 0;
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (connection->rx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    CHECK_ASSERT (rc == 0);

    allocate_connection_buffers (connection->source_device, &connection->tx_buffers, &connection->tx_buffers_mr);
    allocate_connection_buffers (connection->destination_device, &connection->rx_buffers, &connection->rx_buffers_mr);

    /* Initialise the test transfers */
    for (buffer_index = 0; buffer_index < NUM_TEST_BUFFERS_PER_PORT; buffer_index++)
    {
        struct ibv_send_wr *const wr = &connection->wrs[buffer_index];
        struct ibv_sge *const sge = &connection->sges[buffer_index];

        sge->addr = (uintptr_t) connection->tx_buffers->buffers[buffer_index];
        sge->length = TEST_BUFFER_SIZE_BYTES;
        sge->lkey = connection->tx_buffers_mr->lkey;

        wr->num_sge = 1;
        wr->sg_list = sge;
        wr->next = NULL;
        wr->opcode = IBV_WR_RDMA_WRITE;
        wr->send_flags = 0;
        wr->wr.rdma.remote_addr = (uintptr_t) connection->rx_buffers->buffers[buffer_index];
        wr->wr.rdma.rkey = connection->rx_buffers_mr->rkey;
        wr->wr_id = (connection_index << WR_ID_CONNECTION_INDEX_SHIFT) |
                    (buffer_index     << WR_ID_BUFFER_INDEX_SHIFT    );

        initialise_tx_pattern_buffer (connection->tx_buffers->buffers[buffer_index]);
    }

    context->num_connections++;
}



/**
 * @brief Add test connections for all supported RDMA ports on the local host
 * @param[in/out] context The test context being initialised
 */
static void add_connections_for_all_supported_rdma_ports (test_load_context_t *const context)
{
    uint32_t source_device_index = 0;
    uint32_t source_port = 0;
    uint32_t destination_device_index = 0;
    uint32_t destination_port = 0;

    for (uint32_t device_index = 0; device_index < context->num_rdma_devices; device_index++)
    {
        const uint32_t candidate_device_index = device_index;
        rdma_test_device_t *const candidate_device = &context->devices[candidate_device_index];

        /* This program only supports connecting RDMA device which use Infiniband transport.
         *
         * iWARP isn't supported since:
         * a. Requires different code to connect the queue-pairs.
         * b. Think the if iWARP is connected between two ports on the local host, no actual traffic gets sent on the physical ports. */
        if (candidate_device->device->transport_type == IBV_TRANSPORT_IB)
        {
            for (uint32_t candidate_port_index = 1;
                    candidate_port_index <= candidate_device->device_attributes.phys_port_cnt;
                    candidate_port_index++)
            {
                if (source_port == 0)
                {
                    /* Save this is a source port for later use in a connection */
                    source_device_index = candidate_device_index;
                    source_port = candidate_port_index;
                }
                else
                {
                    /* Initialise the full-duplex transfers between the source and destination ports */
                    destination_device_index = candidate_device_index;
                    destination_port = candidate_port_index;
                    initialise_connection_transfers (context, source_device_index, source_port, destination_device_index, destination_port);
                    initialise_connection_transfers (context, destination_device_index, destination_port, source_device_index, source_port);
                    source_device_index = 0;
                    source_port = 0;
                    destination_device_index = 0;
                    destination_port = 0;
                }
            }
        }
        else
        {
            printf ("Skipping %u ports on %s due to transport_type %u not supported by this program\n",
                    candidate_device->device_attributes.phys_port_cnt, candidate_device->device->name,
                    candidate_device->device->transport_type);
        }
    }

    if (source_port != 0)
    {
        const rdma_test_device_t *const unused = &context->devices[source_device_index];

        printf ("Skipping %s port %u as no other port for connection\n", unused->device->name, source_port);
    }
}


/*
 * @brief Read the RDMA port counters for all ports used in the test
 * @param[in-out] context Where to store the port counters
 * @param[in] test_start If true being called to store counters at the start of the test,
 *                       and if false the counters at the end of the test.
 */
static void read_all_port_counters (test_load_context_t *const context, const bool test_start)
{
    uint32_t connection_index;
    uint32_t counter_index;
    char port_counter_pathname[PATH_MAX];
    FILE *counter_file;

    for (connection_index = 0; connection_index < context->num_connections; connection_index++)
    {
        test_load_connection_context_t *const connection = &context->connections[connection_index];
        rdma_port_counters_t *const counters = test_start ?
                &connection->source_port_counters_start : &connection->source_port_counters_end;

        const struct
        {
            const char *const name;
            uint64_t *const value;
        } counter_defs[] =
        {
            {
                .name = "port_xmit_data",
                .value = &counters->tx_words
            },
            {
                .name = "port_rcv_data",
                .value = &counters->rx_words
            }
        };

        for (counter_index = 0; counter_index < (sizeof(counter_defs) / sizeof(counter_defs[0])); counter_index++)
        {
            snprintf (port_counter_pathname, sizeof (port_counter_pathname), "%s/ports/%" PRIu32 "/counters/%s",
                    connection->source_device->device->ibdev_path, connection->source_port, counter_defs[counter_index].name);
            counter_file = fopen (port_counter_pathname, "r");
            CHECK_ASSERT (counter_file != NULL);
            const int num_read = fscanf (counter_file, "%" SCNu64, counter_defs[counter_index].value);
            CHECK_ASSERT (num_read == 1);
            fclose (counter_file);
        }
    }
}


/**
 * @brief Keep the RDMA ports active by queueing RDMA write transfers, until requested to stop.
 * @param[in-out] context The context for the test
 */
static void generate_test_load (test_load_context_t *const context)
{
    uint32_t poll_fd_index;
    uint32_t connection_index;
    uint32_t buffer_index;
    int rc;
    struct ibv_send_wr *bad_wr = NULL;
    struct ibv_cq *cq;
    struct ibv_wc wc;
    void *cq_context;
    struct sigaction action;
    uint32_t num_outstanding_transfers;
    bool test_stopping;

    /* Install a signal handler to allow a request to stop transmission */
    printf ("Press Ctrl-C to stop the RDMA test load\n");
    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_transmission_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    check_assert (rc == 0, "sigaction");

    /* Queue all transfers */
    num_outstanding_transfers = 0;
    rc = clock_gettime (CLOCK_MONOTONIC, &context->test_start_time);
    CHECK_ASSERT (rc == 0);
    for (connection_index = 0; connection_index < context->num_connections; connection_index++)
    {
        test_load_connection_context_t *const connection = &context->connections[connection_index];

        connection->num_completed_transfers = 0;
        for (buffer_index = 0; buffer_index < NUM_TEST_BUFFERS_PER_PORT; buffer_index++)
        {
            rc = ibv_req_notify_cq (connection->source_device->cq, 0);
            CHECK_ASSERT (rc == 0);
            rc = ibv_post_send (connection->tx_qp, &connection->wrs[buffer_index], &bad_wr);
            CHECK_ASSERT (rc == 0);
            num_outstanding_transfers++;
        }
    }

    /* Generate the test load until told to stop, by re-queueing the transfers as they complete.
     * Once told to stop, waits for all queued transfers to complete before exiting the loop. */
    test_stopping = false;
    while (!test_stopping || (num_outstanding_transfers > 0))
    {
        if (stop_transmission)
        {
            test_stopping = true;
        }

        errno = 0;
        rc = poll (context->poll_fds, context->num_poll_fds, -1);
        const int saved_errno = errno;
        if (rc > 0)
        {
            for (poll_fd_index = 0; poll_fd_index < context->num_poll_fds; poll_fd_index++)
            {
                if (context->poll_fds[poll_fd_index].revents & POLLIN)
                {
                    /* Read which transfer has completed */
                    rdma_test_device_t *const rdma_device = &context->devices[context->poll_fds_device_indicies[poll_fd_index]];
                    rc = ibv_get_cq_event (rdma_device->completion_channel, &cq, &cq_context);
                    CHECK_ASSERT (rc == 0);
                    rc = ibv_poll_cq (cq, 1, &wc);
                    CHECK_ASSERT (rc == 1);
                    CHECK_ASSERT (wc.status == IBV_WC_SUCCESS);
                    ibv_ack_cq_events (cq, 1);

                    /* Decode the wr_id to determine which device / port / buffer the transfer was for */
                    connection_index = (wc.wr_id >> WR_ID_CONNECTION_INDEX_SHIFT) & WR_ID_CONNECTION_INDEX_MASK;
                    buffer_index = (wc.wr_id >> WR_ID_BUFFER_INDEX_SHIFT) & WR_ID_BUFFER_INDEX_MASK;
                    CHECK_ASSERT (connection_index < context->num_connections);

                    /* Record transfer completion */
                    test_load_connection_context_t *const connection = &context->connections[connection_index];
                    CHECK_ASSERT (rdma_device == connection->source_device);
                    rc = clock_gettime (CLOCK_MONOTONIC, &connection->last_completion_time);
                    CHECK_ASSERT (rc == 0);
                    connection->num_completed_transfers++;
                    num_outstanding_transfers--;

                    /* Request further completion notification */
                    rc = ibv_req_notify_cq (cq, 0);
                    CHECK_ASSERT (rc == 0);

                    /* Re-queue the transfer, unless the test is stopping */
                    if (!test_stopping)
                    {
                        rc = ibv_post_send (connection->tx_qp, &connection->wrs[buffer_index], &bad_wr);
                        CHECK_ASSERT (rc == 0);
                        num_outstanding_transfers++;
                    }
                }
            }
        }
        else
        {
            CHECK_ASSERT (saved_errno == EINTR);
        }
    }
}


/*
 * @brief Display a summary of how much data has been transferred for each RDMA port
 */
static void display_test_summary (const test_load_context_t *const context)
{
    uint32_t connection_index;
    const int64_t nsecs_per_sec = 1000000000;
    const int64_t start_ns = (context->test_start_time.tv_sec * nsecs_per_sec) + context->test_start_time.tv_nsec;
    int compare_result;

    printf ("\n");
    for (connection_index = 0; connection_index < context->num_connections; connection_index++)
    {
        const test_load_connection_context_t *const connection = &context->connections[connection_index];

        /* Perform a check that the receive buffers match the contents of the transmit buffers.
         * Since generate_test_load() starts by queueing all transfers, and waits for all transfers to complete before exiting,
         * all of the rx_buffers should contain valid data regardless of how quickly a request was made to stop the test.
         * As this is not expected to fail, doesn't attempt to indicate where the comparison fails. */
        compare_result = memcmp (connection->rx_buffers->buffers, connection->tx_buffers->buffers,
                sizeof (connection->tx_buffers->buffers));
        printf ("%s port %u rx_buffer compare : %s\n", connection->destination_device->device->name, connection->destination_port,
                (compare_result == 0) ? "PASS" : "FAIL");

        const uint64_t total_rdma_bytes = (uint64_t) connection->num_completed_transfers * TEST_BUFFER_SIZE_BYTES;
        const int64_t stop_ns = (connection->last_completion_time.tv_sec * nsecs_per_sec) + connection->last_completion_time.tv_nsec;
        const double duration_secs = (stop_ns - start_ns) / 1E9;
        const uint64_t port_tx_bytes = sizeof (uint32_t) *
                (connection->source_port_counters_end.tx_words - connection->source_port_counters_start.tx_words);
        const uint64_t port_rx_bytes = sizeof (uint32_t) *
                (connection->source_port_counters_end.rx_words - connection->source_port_counters_start.rx_words);

        /* Display the amount of data transferred by RDMA writes, from the total number of transfers and
         * the size of each transfer */
        printf ("%s port %" PRIu32 " -> %s port %" PRIu32 " RDMA write transmitted %" PRIu64 " bytes in %.6f seconds, %.1f Mbytes/sec\n",
                connection->source_device->device->name, connection->source_port,
                connection->destination_device->device->name, connection->destination_port,
                total_rdma_bytes, duration_secs, (total_rdma_bytes / duration_secs) / 1E6);

        /* Display the amount of bytes transmitted and received on the RDMA ports, which includes overheads */
        printf ("%s port %" PRIu32 " type %s transmitted %" PRIu64 " bytes in %.6f seconds, %.1f Mbytes/sec\n",
                connection->source_device->device->name, connection->source_port,
                gid_type_names[connection->source_gid_entry.gid_type],
                port_tx_bytes, duration_secs, (port_tx_bytes / duration_secs) / 1E6);
        printf ("%s port %" PRIu32 " type %s received %" PRIu64 " bytes in %.6f seconds, %.1f Mbytes/sec\n",
                connection->source_device->device->name, connection->source_port,
                gid_type_names[connection->destination_gid_entry.gid_type],
                port_rx_bytes, duration_secs, (port_rx_bytes / duration_secs) / 1E6);
    }
}


int main (int argc, char *argv[])
{
    char junk;
    int rc;
    test_load_context_t context;

    rc = numa_available ();
    CHECK_ASSERT (rc != -1);

    /* Add protection against fork() being called */
    rc = ibv_fork_init ();
    CHECK_ASSERT (rc == 0);

    get_local_rdma_devices (&context);

    /* Parse command line arguments */
    if ((argc < 2) || (argc > 3))
    {
        fprintf (stderr, "Usage: %s <numa_node_offset> [<ethernet_gid_index>]\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    if (sscanf (argv[1], "%d%c", &arg_numa_node_offset, &junk) != 1)
    {
        fprintf (stderr, "Out of range <numa_node_offset>\n");
        exit (EXIT_FAILURE);
    }

    if (argc == 3)
    {
        uint32_t gid_index;

        if ((sscanf (argv[2], "%u%c", &gid_index, &junk) != 1) || (gid_index > UINT8_MAX))
        {
            fprintf (stderr, "Out of range <ethernet_gid_index>\n");
            exit (EXIT_FAILURE);
        }
        arg_ethernet_gid_index = (uint8_t) gid_index;
    }

    add_connections_for_all_supported_rdma_ports (&context);

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    bool test_start = true;
    read_all_port_counters (&context, test_start);
    generate_test_load (&context);
    test_start = false;
    read_all_port_counters (&context, test_start);
    display_test_summary (&context);

    return EXIT_SUCCESS;
}
