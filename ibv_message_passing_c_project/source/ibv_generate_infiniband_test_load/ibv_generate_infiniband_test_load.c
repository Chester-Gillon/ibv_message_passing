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
 *   This program assumes there are one or more dual-port Infiniband HCAs fitted, and for each Infiniband HCA attempts a full
 *   duplex transfer from port 1 -> 2 as well as port 2 -> 1. By effectively looping back the ports on each local
 *   dual port Infiniband HCA via a connected switch the program can be run a single instance on a host to exercise all local
 *   Infiniband ports independent of any other hosts.
 *
 *   If run on a host two dual-port Infiniband HCAs then will queue transmission as fast as possible for the following
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
 * Uses large individual buffers to minimise the CPU overhead of keeping the Infiniband RDMA busy.
 * Total size per port sufficient for a PRBS32 pattern. */
#define TEST_BUFFER_SIZE_BYTES 0x10000000
#define TEST_BUFFER_SIZE_WORDS (TEST_BUFFER_SIZE_BYTES / sizeof (uint32_t))
#define NUM_TEST_BUFFERS_PER_PORT 2


/* Used to size arrays of the Infiniband devices used in the test */
#define MAX_INFINIBAND_DEVICES 2

/* Program written assuming dual port Infiniband devices are present */
#define EXPECTED_PORTS_PER_DEVICE 2

/* Used to build a wr_id which identifies which device / port / buffer a RDMA write work-request is for.
 * These as used so that a work-request completed can determine which request to re-queue */
#define WR_ID_DEVICE_INDEX_SHIFT   2
#define WR_ID_DEVICE_INDEX_MASK  0x1
#define WR_ID_PORT_INDEX_SHIFT     1
#define WR_ID_PORT_INDEX_MASK    0x1
#define WR_ID_BUFFER_INDEX_SHIFT   0
#define WR_ID_BUFFER_INDEX_MASK  0x1


/* Used to allocate space for all transmit and receive buffers used for one Infiniband device */
typedef struct
{
    uint32_t tx_buffers[EXPECTED_PORTS_PER_DEVICE][NUM_TEST_BUFFERS_PER_PORT][TEST_BUFFER_SIZE_WORDS];
    uint32_t rx_buffers[EXPECTED_PORTS_PER_DEVICE][NUM_TEST_BUFFERS_PER_PORT][TEST_BUFFER_SIZE_WORDS];
} test_load_device_buffers;


/* Used to obtain the Infiniband port counts from one of the ports under test */
typedef struct
{
    /* From IB_PC_EXT_XMT_BYTES_F, which is actual a count of 32-bit words */
    uint64_t tx_words;
    /* From IB_PC_EXT_RCV_BYTES_F, which is actual a count of 32-bit words */
    uint64_t rx_words;
} infiniband_port_counters;


/* The context for one pair of ports on an Infiniband device used for the test,
 * used to perform RDMA writes from the source to destination port.
 * Since the test generates a full-duplex test load for each Infiniband device there are two of these contexts, with the
 * source and destination ports swapped. */
typedef struct
{
    /* The source and destination ports used for this context */
    uint8_t source_port;
    uint8_t destination_port;
    /* The attribut4es of the ports used for the transfers */
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
    /* The Infiniband counters for the source_post at the start and end of the test, for correlating the amount of traffic
     * on the Infiniband ports against the total transferred by RDMA writes. */
    infiniband_port_counters source_port_counters_start;
    infiniband_port_counters source_port_counters_end;
} test_load_port_context_t;


/* The context for one Infiniband device used for the test */
typedef struct
{
    /* The Infiniband device */
    struct ibv_context *device;
    struct ibv_device_attr device_attributes;
    /* The zero-based index of this Infiniband device used when setting the wr_id for the transfers */
    uint32_t device_index;
    /* The local NUMA node for the Infiniband device */
    int device_numa_node;
    /* The protection domain for the device */
    struct ibv_pd *device_pd;
    /* The completion queue used for RDMA writes on all ports, so the main thread can be used to block waiting for
     * completion. */
    struct ibv_cq *cq;
    /* The completion channel used to wait for completions in cq */
    struct ibv_comp_channel *completion_channel;
    /* The allocated buffers for this device */
    test_load_device_buffers *buffers;
    /* Which NUMA node buffers is allocated on */
    struct bitmask *buffers_numa_mask;
    /* The Infiniband memory regions to access all of buffers */
    struct ibv_mr *buffers_mr;
    /* Used to generate a full duplex test load of the ports on the Infiniband device */
    test_load_port_context_t ports[EXPECTED_PORTS_PER_DEVICE];
} test_load_device_context_t;


/* The context for the test */
typedef struct
{
    /* The number of Infiniband devices present */
    int num_devices;
    /* The list of Infiniband devices */
    struct ibv_device **device_list;
    /* The Infiniband devices used for the context */
    test_load_device_context_t devices[MAX_INFINIBAND_DEVICES];
    /* File descriptors used to block waiting for completion from any device */
    struct pollfd poll_fds[MAX_INFINIBAND_DEVICES];
    /* The CLOCK_MONOTONIC time at which started to queue the test transfers */
    struct timespec test_start_time;
} test_load_context_t;


/* Starting valid for PRBS32 test pattern. Any non-zero value sufficient */
#define PRBS32_PATTERN_START 1u


/** Set from a signal handler to request that the transmission of the test load stops */
static volatile bool stop_transmission;


/* Command line argument which defines the offset between the Infiniband device NUMA node and the NUMA node in which the
 * RDMA buffers are placed. */
static int arg_numa_node_offset;


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


/**
 * @brief Initialise the transfers for one pair of Infiniband ports to generate a test load
 * @details Since the source and destination ports are in the same PC and accessed by this program, the queue-pairs can be
 *          initialised with a simple sequence without requiring any out-of-band communication of the queue-pair attributes.
 * @param device[in/out] The Infiniband device context to initialise the test transfers for.
 * @param port_index[out] The index into device->ports[] to initialise
 * @param source_port[in] The port on the Infiniband to use for transmit
 * @param destination_port[in] The port on the Infiniband to use for receive
 */
static void initialise_test_transfers (test_load_device_context_t *const device, uint32_t port_index,
                                       const uint8_t source_port, const uint8_t destination_port)
{
    const uint32_t tx_psn = get_random_psn ();
    const uint32_t rx_psn = get_random_psn ();
    test_load_port_context_t *const port = &device->ports[port_index];
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;
    uint32_t buffer_index;

    /* Default values */
    const uint8_t timeout = 16;
    const uint8_t retry_cnt = 7;

    port->source_port = source_port;
    port->destination_port = destination_port;
    rc = ibv_query_port (device->device, source_port, &port->tx_port_attributes);
    CHECK_ASSERT (rc == 0);
    rc = ibv_query_port (device->device, destination_port, &port->rx_port_attributes);
    CHECK_ASSERT (rc == 0);

    /* Create queue-pairs */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = device->cq;
    qp_init_attr.recv_cq = device->cq;
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 1;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = true;
    port->tx_qp = ibv_create_qp (device->device_pd, &qp_init_attr);
    CHECK_ASSERT (port->tx_qp != NULL);

    qp_init_attr.cap.max_send_wr = 0;
    qp_init_attr.cap.max_send_sge = 0;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    port->rx_qp = ibv_create_qp (device->device_pd, &qp_init_attr);
    CHECK_ASSERT (port->rx_qp != NULL);

    /* Transition queue-pairs to the INIT state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = source_port;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (port->tx_qp, &qp_attr,
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
    rc = ibv_modify_qp (port->rx_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    CHECK_ASSERT (rc == 0);

    /* Transition the queue-pairs to the Ready-to-Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = port->tx_port_attributes.active_mtu;
    qp_attr.dest_qp_num = port->rx_qp->qp_num;
    qp_attr.rq_psn = rx_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    if (port->rx_port_attributes.link_layer == IBV_LINK_LAYER_ETHERNET)
    {
        /* @todo When the link level is Ethernet assume GID index zero is for RoCEv1.
         *       Support for RoCE was tested on Mellanox Connect-X2 VPI cards which only support RoCEv1.
         *
         *       Later versions of rdma-core have ibv_query_gid_type() which could be used to search for a specific RoCE version.
         *
         *       There is also /sys/class/infiniband/<device>/ports/<port_number>/gid_attrs/types/<gid_index> which has a string
         *       for the RoCE version for the GID index on a given index of a port.
         *       Older Kernels, e.g. 3.10.33-rt32.33.el6rt.x86_64, may not have the gid_attrs files in which only RoCEv1
         *       is supported.
         */
        qp_attr.ah_attr.is_global = true;
        qp_attr.ah_attr.grh.sgid_index = 0;
        rc = ibv_query_gid (device->device, destination_port, qp_attr.ah_attr.grh.sgid_index,
                &qp_attr.ah_attr.grh.dgid);
        CHECK_ASSERT (rc == 0);
        qp_attr.ah_attr.grh.hop_limit = 1;
    }
    else
    {
        /* For Infiniband use LID addressing */
        qp_attr.ah_attr.is_global = false;
    }
    qp_attr.ah_attr.dlid = port->rx_port_attributes.lid;
    qp_attr.ah_attr.sl = 0;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = source_port;
    rc = ibv_modify_qp (port->tx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    CHECK_ASSERT (rc == 0);

    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = port->rx_port_attributes.active_mtu;
    qp_attr.dest_qp_num = port->tx_qp->qp_num;
    qp_attr.rq_psn = tx_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    if (port->tx_port_attributes.link_layer == IBV_LINK_LAYER_ETHERNET)
    {
        /* @todo When the link level is Ethernet assume GID index zero is for RoCEv1.
         *       Support for RoCE was tested on Mellanox Connect-X2 VPI cards which only support RoCEv1.
         *
         *       Later versions of rdma-core have ibv_query_gid_type() which could be used to search for a specific RoCE version.
         *
         *       There is also /sys/class/infiniband/<device>/ports/<port_number>/gid_attrs/types/<gid_index> which has a string
         *       for the RoCE version for the GID index on a given index of a port.
         *       Older Kernels, e.g. 3.10.33-rt32.33.el6rt.x86_64, may not have the gid_attrs files in which only RoCEv1
         *       is supported.
         */
        qp_attr.ah_attr.is_global = true;
        qp_attr.ah_attr.grh.sgid_index = 0;
        rc = ibv_query_gid (device->device, source_port, qp_attr.ah_attr.grh.sgid_index,
                &qp_attr.ah_attr.grh.dgid);
        CHECK_ASSERT (rc == 0);
        qp_attr.ah_attr.grh.hop_limit = 1;
    }
    else
    {
        /* For Infiniband use LID addressing */
        qp_attr.ah_attr.is_global = false;
    }
    qp_attr.ah_attr.dlid = port->tx_port_attributes.lid;
    qp_attr.ah_attr.sl = 0;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = destination_port;
    rc = ibv_modify_qp (port->rx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    CHECK_ASSERT (rc == 0);

    /* Transition the queue-pairs to the Ready-to-Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = tx_psn;
    qp_attr.timeout = timeout;
    qp_attr.retry_cnt = retry_cnt;
    qp_attr.rnr_retry = 0;
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (port->tx_qp, &qp_attr,
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
    rc = ibv_modify_qp (port->rx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    CHECK_ASSERT (rc == 0);

    /* Initialise the test transfers */
    for (buffer_index = 0; buffer_index < NUM_TEST_BUFFERS_PER_PORT; buffer_index++)
    {
        struct ibv_send_wr *const wr = &port->wrs[buffer_index];
        struct ibv_sge *const sge = &port->sges[buffer_index];

        sge->addr = (uintptr_t) device->buffers->tx_buffers[port_index][buffer_index];
        sge->length = TEST_BUFFER_SIZE_BYTES;
        sge->lkey = device->buffers_mr->lkey;

        wr->num_sge = 1;
        wr->sg_list = sge;
        wr->next = NULL;
        wr->opcode = IBV_WR_RDMA_WRITE;
        wr->send_flags = 0;
        wr->wr.rdma.remote_addr = (uintptr_t) device->buffers->rx_buffers[port_index][buffer_index];
        wr->wr.rdma.rkey = device->buffers_mr->rkey;
        wr->wr_id = (device->device_index << WR_ID_DEVICE_INDEX_SHIFT) |
                    (port_index           << WR_ID_PORT_INDEX_SHIFT  ) |
                    (buffer_index         << WR_ID_BUFFER_INDEX_SHIFT);

        initialise_tx_pattern_buffer (device->buffers->tx_buffers[port_index][buffer_index]);
    }
}


/**
 * @brief Initialise the test transfers for all Infiniband devices in the local PC.
 * @param[out] context The test context to initialise
 */
static void initialise_test_load_context (test_load_context_t *const context)
{
    uint32_t device_index;
    int rc;
    int buffers_numa_node;
    long lrc;
    char sysfs_path[PATH_MAX];
    FILE *sysfs_file;
    const int pagesize = getpagesize ();

    context->device_list = ibv_get_device_list (&context->num_devices);
    check_assert (context->num_devices > 0, "No Infiniband devices found");

    for (device_index = 0; device_index < context->num_devices; device_index++)
    {
        test_load_device_context_t *const device = &context->devices[device_index];

        /* Open the device and allocate the protection domain */
        device->device_index = device_index;
        device->device = ibv_open_device (context->device_list[device_index]);
        CHECK_ASSERT (device->device != NULL);
        device->device_pd = ibv_alloc_pd (device->device);
        CHECK_ASSERT (device->device_pd != NULL);
        rc = ibv_query_device (device->device, &device->device_attributes);
        CHECK_ASSERT (rc == 0);
        CHECK_ASSERT (device->device_attributes.phys_port_cnt == EXPECTED_PORTS_PER_DEVICE);

        /* Create the completion queue using a completion channel which will be used to block waiting for completion */
        device->completion_channel = ibv_create_comp_channel (device->device);
        CHECK_ASSERT (device->completion_channel != NULL);
        context->poll_fds[device_index].fd = device->completion_channel->fd;
        context->poll_fds[device_index].events = POLLIN;
        device->cq = ibv_create_cq (device->device, EXPECTED_PORTS_PER_DEVICE * NUM_TEST_BUFFERS_PER_PORT, NULL,
                device->completion_channel, 0);
        CHECK_ASSERT (device->cq != NULL);

        /* Find the local NUMA for the Infiniband device */
        snprintf (sysfs_path, sizeof (sysfs_path), "%s/device/numa_node", device->device->device->ibdev_path);
        sysfs_file = fopen (sysfs_path, "r");
        const int num_items = fscanf (sysfs_file, "%d", &device->device_numa_node);
        CHECK_ASSERT (num_items == 1);
        fclose (sysfs_file);

        /* Allocate the transmit and receive buffers for use by all ports on the Infinband device */
        rc = posix_memalign ((void **) &device->buffers, pagesize, sizeof (test_load_device_buffers));
        CHECK_ASSERT (rc == 0);
        if (device->device_numa_node != -1)
        {
            /* When the computer has multiple NUMA nodes, bind the transmit and receive buffers to a NUMA node
             * with a relative offset to that of the NUMA node local to the Infiniband device. */
            device->buffers_numa_mask = numa_allocate_nodemask ();
            CHECK_ASSERT (device->buffers_numa_mask != NULL);
            buffers_numa_node = (device->device_numa_node + arg_numa_node_offset) % numa_num_configured_nodes ();
            numa_bitmask_setbit (device->buffers_numa_mask, buffers_numa_node);
            lrc = mbind (device->buffers, sizeof (test_load_device_buffers), MPOL_F_STATIC_NODES | MPOL_BIND,
                    device->buffers_numa_mask->maskp, device->buffers_numa_mask->size, MPOL_MF_STRICT | MPOL_MF_MOVE);
            CHECK_ASSERT (lrc == 0);
        }

        device->buffers_mr = ibv_reg_mr (device->device_pd, device->buffers, sizeof (test_load_device_buffers),
                IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
        CHECK_ASSERT (device->buffers_mr != NULL);

        /* Initialise the full-duplex transfers between the ports on this Infiniband device */
        initialise_test_transfers (device, 0, 1, 2);
        initialise_test_transfers (device, 1, 2, 1);
    }
}


/*
 * @brief Read the Infiniband port counters for all ports used in the test
 * @param[in-out] context Where to store the port counters
 * @param[in] test_start If true being called to store counters at the start of the test,
 *                       and if false the counters at the end of the test.
 */
static void read_all_port_counters (test_load_context_t *const context, const bool test_start)
{
    uint32_t device_index;
    uint32_t port_index;
    uint32_t counter_index;
    char port_counter_pathname[PATH_MAX];
    FILE *counter_file;

    for (device_index = 0; device_index < context->num_devices; device_index++)
    {
        test_load_device_context_t *const device = &context->devices[device_index];

        for (port_index = 0; port_index < EXPECTED_PORTS_PER_DEVICE; port_index++)
        {
            test_load_port_context_t *const port = &device->ports[port_index];
            infiniband_port_counters *const counters = test_start ?
                    &port->source_port_counters_start : &port->source_port_counters_end;

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
                        device->device->device->ibdev_path, port_index + 1, counter_defs[counter_index].name);
                counter_file = fopen (port_counter_pathname, "r");
                CHECK_ASSERT (counter_file != NULL);
                const int num_read = fscanf (counter_file, "%" SCNu64, counter_defs[counter_index].value);
                CHECK_ASSERT (num_read == 1);
                fclose (counter_file);
            }
        }
    }
}


/**
 * @brief Keep the Infiniband ports active by queueing Infiniband RDMA write transfers, until requested to stop.
 * @param[in-out] context The context for the test
 */
static void generate_test_load (test_load_context_t *const context)
{
    uint32_t decoded_device_index;
    uint32_t device_index;
    uint32_t port_index;
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
    printf ("Press Ctrl-C to stop the Infiniband test load\n");
    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_transmission_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    check_assert (rc == 0, "sigaction");

    /* Queue all transfers */
    num_outstanding_transfers = 0;
    rc = clock_gettime (CLOCK_MONOTONIC, &context->test_start_time);
    CHECK_ASSERT (rc == 0);
    for (device_index = 0; device_index < context->num_devices; device_index++)
    {
        test_load_device_context_t *const device = &context->devices[device_index];

        for (port_index = 0; port_index < EXPECTED_PORTS_PER_DEVICE; port_index++)
        {
            test_load_port_context_t *const port = &device->ports[port_index];

            port->num_completed_transfers = 0;
            for (buffer_index = 0; buffer_index < NUM_TEST_BUFFERS_PER_PORT; buffer_index++)
            {
                rc = ibv_req_notify_cq (device->cq, 0);
                CHECK_ASSERT (rc == 0);
                rc = ibv_post_send (port->tx_qp, &port->wrs[buffer_index], &bad_wr);
                CHECK_ASSERT (rc == 0);
                num_outstanding_transfers++;
            }
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
        rc = poll (context->poll_fds, context->num_devices, -1);
        const int saved_errno = errno;
        if (rc > 0)
        {
            for (device_index = 0; device_index < context->num_devices; device_index++)
            {
                test_load_device_context_t *const device = &context->devices[device_index];

                if (context->poll_fds[device_index].revents & POLLIN)
                {
                    /* Read which transfer has completed */
                    rc = ibv_get_cq_event (device->completion_channel, &cq, &cq_context);
                    CHECK_ASSERT (rc == 0);
                    rc = ibv_poll_cq (cq, 1, &wc);
                    CHECK_ASSERT (rc == 1);
                    CHECK_ASSERT (wc.status == IBV_WC_SUCCESS);
                    ibv_ack_cq_events (cq, 1);

                    /* Decode the wr_id to determine which device / port / buffer the transfer was for */
                    decoded_device_index = (wc.wr_id >> WR_ID_DEVICE_INDEX_SHIFT) & WR_ID_DEVICE_INDEX_MASK;
                    port_index = (wc.wr_id >> WR_ID_PORT_INDEX_SHIFT) & WR_ID_PORT_INDEX_MASK;
                    buffer_index = (wc.wr_id >> WR_ID_BUFFER_INDEX_SHIFT) & WR_ID_BUFFER_INDEX_MASK;
                    CHECK_ASSERT (decoded_device_index == device_index);

                    /* Record transfer completion */
                    test_load_port_context_t *const port = &device->ports[port_index];
                    rc = clock_gettime (CLOCK_MONOTONIC, &port->last_completion_time);
                    CHECK_ASSERT (rc == 0);
                    port->num_completed_transfers++;
                    num_outstanding_transfers--;

                    /* Request further completion notification */
                    rc = ibv_req_notify_cq (cq, 0);
                    CHECK_ASSERT (rc == 0);

                    /* Re-queue the transfer, unless the test is stopping */
                    if (!test_stopping)
                    {
                        rc = ibv_post_send (port->tx_qp, &port->wrs[buffer_index], &bad_wr);
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
 * @brief Display a summary of how much data has been transferred for each Infiniband port
 */
static void display_test_summary (const test_load_context_t *const context)
{
    uint32_t device_index;
    uint32_t port_index;
    const int64_t nsecs_per_sec = 1000000000;
    const int64_t start_ns = (context->test_start_time.tv_sec * nsecs_per_sec) + context->test_start_time.tv_nsec;
    int compare_result;

    printf ("\n");
    for (device_index = 0; device_index < context->num_devices; device_index++)
    {
        const test_load_device_context_t *const device = &context->devices[device_index];

        /* Perform a check that the receive buffers match the contents of the transmit buffers.
         * Since generate_test_load() starts by queueing all transfers, and waits for all transfers to complete before exiting,
         * all of the rx_buffers should contain valid data regardless of how quickly a request was made to stop the test.
         * As this is not expected to fail, doesn't attempt to indicate where the comparison fails. */
        compare_result = memcmp (device->buffers->rx_buffers, device->buffers->tx_buffers, sizeof (device->buffers->tx_buffers));
        printf ("%s rx_buffer compare : %s\n", device->device->device->name, (compare_result == 0) ? "PASS" : "FAIL");

        for (port_index = 0; port_index < EXPECTED_PORTS_PER_DEVICE; port_index++)
        {
            const test_load_port_context_t *const port = &device->ports[port_index];
            const uint64_t total_rdma_bytes = (uint64_t) port->num_completed_transfers * TEST_BUFFER_SIZE_BYTES;
            const int64_t stop_ns = (port->last_completion_time.tv_sec * nsecs_per_sec) + port->last_completion_time.tv_nsec;
            const double duration_secs = (stop_ns - start_ns) / 1E9;
            const uint64_t port_tx_bytes = sizeof (uint32_t) *
                    (port->source_port_counters_end.tx_words - port->source_port_counters_start.tx_words);
            const uint64_t port_rx_bytes = sizeof (uint32_t) *
                    (port->source_port_counters_end.rx_words - port->source_port_counters_start.rx_words);

            /* Display the amount of data transferred by RDMA writes, from the total number of transfers and
             * the size of each transfer */
            printf ("%s port %" PRIu32 " -> %" PRIu32 " RDMA write transmitted %" PRIu64 " bytes in %.6f seconds, %.1f Mbytes/sec\n",
                    device->device->device->name, port->source_port, port->destination_port,
                    total_rdma_bytes, duration_secs, (total_rdma_bytes / duration_secs) / 1E6);

            /* Display the amount of bytes transmitted and received on the Infiniband ports, which includes overheads */
            printf ("%s port %" PRIu32 " transmitted %" PRIu64 " bytes in %.6f seconds, %.1f Mbytes/sec\n",
                    device->device->device->name, port->source_port,
                    port_tx_bytes, duration_secs, (port_tx_bytes / duration_secs) / 1E6);
            printf ("%s port %" PRIu32 " received %" PRIu64 " bytes in %.6f seconds, %.1f Mbytes/sec\n",
                    device->device->device->name, port->source_port,
                    port_rx_bytes, duration_secs, (port_rx_bytes / duration_secs) / 1E6);
        }
    }
}


int main (int argc, char *argv[])
{
    char junk;
    int rc;
    test_load_context_t context;

    /* Parse command line arguments */
    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s <numa_node_offset>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    if (sscanf (argv[1], "%d%c", &arg_numa_node_offset, &junk) != 1)
    {
        fprintf (stderr, "Out of range <numa_node_offset>\n");
        exit (EXIT_FAILURE);
    }

    rc = numa_available ();
    CHECK_ASSERT (rc != -1);

    /* Add protection against fork() being called */
    rc = ibv_fork_init ();
    CHECK_ASSERT (rc == 0);

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    memset (&context, 0, sizeof (context));
    initialise_test_load_context (&context);

    bool test_start = true;
    read_all_port_counters (&context, test_start);
    generate_test_load (&context);
    test_start = false;
    read_all_port_counters (&context, test_start);
    display_test_summary (&context);

    return EXIT_SUCCESS;
}
