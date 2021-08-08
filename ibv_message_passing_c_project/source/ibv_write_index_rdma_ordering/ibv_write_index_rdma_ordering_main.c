/*
 * @file ibv_write_index_rdma_ordering_main.c
 * @date 7 Aug 2021
 * @author Chester Gillon
 * @brief Test to measure the RDMA ordering when multiple transfers are queued to one index location
 * @details Uses RDMA to transfer data from a writer SMB to read SMB where the SMB has:
 *          a. An array of data blocks.
 *          b. A write index which is incremented.
 *
 *          The reader polls for a change in the write index, and then checks for the expected data block contents.
 *
 *          Each transfer involves a pair of IBV_WR_RDMA_WRITE work-requests:
 *          a. First for the data block, which has a different address for each block.
 *          b. Second on the write index, which has the same address for all blocks but a different value.
 *
 *          The data block are not sent inline, but the write index has the option of being sent inline.
 *
 *          By having multiple transfers queued, and running with and without the write index sent inline,
 *          allows a check on if the reader can sample a write index value which indicates the data block
 *          should be available but isn't.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <time.h>

#include <unistd.h>
#include <infiniband/verbs.h>
#include <sys/mman.h>


/** The assumed cache line size for allocating areas. Should be valid for all Sandy Bridge and Haswell processors */
#define CACHE_LINE_SIZE_BYTES 64


/** Defines a fixed size block of data transferred during the test.
 *  Only the sequence_number is populated by the test, which the reader uses to check that the expected data has been
 *  written to memory by the time the write_index indicates the data block should be populated.
 *
 *  The payload just pushes the data block size to be larger than the maximum inline size for a ConnectX device (28 bytes). */
typedef struct __attribute__((aligned(CACHE_LINE_SIZE_BYTES)))
{
    uint32_t sequence_number;
    char payload[CACHE_LINE_SIZE_BYTES - sizeof (uint32_t)];
} data_block_t;


/** Defines the layout of a SMB accessed by RDMA during the test.
 *  This is mapped on the writer and reader threads, with RDMA used to transfer from the writer -> reader */
#define NUM_DATA_BLOCKS 200000
typedef struct
{
    /** Advanced to indicate a new data block is available, in the data_blocks[] index preceding the value of write_index.
     *  global_read_index is the data_block[] which the reader will read next. */
    uint32_t write_index __attribute__((aligned(CACHE_LINE_SIZE_BYTES)));
    /** Circular array of data blocks written by the test.
     *  For this test all data blocks are the same size, which allows the write_index to directly reference them.
     *  Could be expanded to allow variable size data blocks and write_index could then be a byte index. */
    data_block_t data_blocks[NUM_DATA_BLOCKS];
} smb_t;


/** Defines one RDMA endpoint used during the test */
typedef struct
{
    struct ibv_context *rdma_device;
    uint8_t port_num;
    struct ibv_device_attr device_attributes;
    struct ibv_port_attr port_attributes;
    struct ibv_pd *device_pd;
    smb_t *smb;
    struct ibv_mr *mr;
    struct ibv_cq *cq;
    struct ibv_qp *qp;
    uint32_t initial_psn;
} rdma_endpoint_t;


/** Defines one RDMA queue entry to transfer a data block and an update to the write index */
typedef struct
{
    struct ibv_sge data_block_sge;
    struct ibv_sge write_index_sge;
    struct ibv_send_wr data_block_wr;
    struct ibv_send_wr write_index_wr;
} write_queue_entry_t;


/** Defines the context for one writer thread */
typedef struct
{
    rdma_endpoint_t local_endpoint;
    const rdma_endpoint_t *remote_endpoint;
    bool send_index_inline;
    uint32_t write_queue_length;
    uint32_t test_num_data_writes;
    write_queue_entry_t *write_queue;
    uint32_t num_write_queue_entries_in_use;
    uint32_t max_write_queue_entries_in_use;
    struct ibv_wc *wcs;
    uint32_t queue_index;
    uint32_t write_index;
    uint32_t sampled_read_index;
    uint32_t sequence_number;
} write_thread_context_t;


/** Defines one data block in which the reader had to wait for the expected sequence number */
typedef struct
{
    /** Identifies which block in the test */
    uint32_t data_num;
    /** The expected sequence number in the data block */
    uint32_t expected_sequence_number;
    /** The total number of polls until the expected sequence number was seen */
    uint32_t total_polls;
} read_unexpected_sequence_number_t;


/** Defines the context for one reader thread */
typedef struct
{
    rdma_endpoint_t local_endpoint;
    uint32_t test_num_data_writes;
    uint32_t expected_sequence_number;
    uint32_t read_index;
    uint32_t sampled_write_index;
    read_unexpected_sequence_number_t *unexpected_sequence_numbers;
    uint32_t num_unexpected_sequence_numbers;
} read_thread_context_t;


/** Used to indicate the reader is ready to poll for data */
static bool reader_ready;


/** Published by the read thread to allow the write thread to track available data blocks */
static uint32_t global_read_index;


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
 * @brief Perform a cache line aligned allocation, where the allocation is zero-initialised.
 * @param[in] alloc_size The required allocation size in bytes.
 * @return Pointer to the allocated memory
 */
static void *cache_line_aligned_calloc (const size_t alloc_size)
{
    const size_t aligned_size = ((alloc_size + CACHE_LINE_SIZE_BYTES - 1) / CACHE_LINE_SIZE_BYTES) * CACHE_LINE_SIZE_BYTES;
    void *buffer = NULL;
    int rc;

    rc = posix_memalign (&buffer, CACHE_LINE_SIZE_BYTES, aligned_size);
    CHECK_ASSERT (rc == 0);
    memset (buffer, 0, aligned_size);

    return buffer;
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
 * @brief Open a RDMA endpoint for the test, performing initialisation local to the endpoint
 * @param[in] num_ibv_devices The number of RDMA devices on the local board
 * @param[in] device_list The list of RDMA devices on the local board
 * @param[in] dev_name The name of the RDMA device to use for the endpoint
 * @param[in] port_num The port number of the RMDA device to use for the endpoint
 * @param[out] endpoint The initialised endpoint
 * @param[in] is_write_end Indicates if being called for the write or read endpoint
 * @param[in] write_queue_length The length of the write queue, used to size completion queues
 */
static void open_rdma_endpoint (const int num_ibv_devices, struct ibv_device **const device_list,
                                const char *const dev_name, const uint8_t port_num,
                                rdma_endpoint_t *const endpoint,
                                const bool is_write_end, const uint32_t write_queue_length)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int device_index;
    int rc;

    /* Locate the requested RDMA device and port */
    endpoint->port_num = port_num;
    endpoint->rdma_device = NULL;
    for (device_index = 0; (endpoint->rdma_device == NULL) && (device_index < num_ibv_devices); device_index++)
    {
        if (strcmp (dev_name, ibv_get_device_name (device_list[device_index])) == 0)
        {
            endpoint->rdma_device = ibv_open_device (device_list[device_index]);
            CHECK_ASSERT (endpoint->rdma_device != NULL);
            rc = ibv_query_device (endpoint->rdma_device, &endpoint->device_attributes);
            CHECK_ASSERT (rc == 0);
        }
    }

    if ((endpoint->rdma_device == NULL) || (port_num == 0) || (port_num > endpoint->device_attributes.phys_port_cnt))
    {
        fprintf (stderr, "Unable to find RDMA device %s port %u\n", dev_name, port_num);
        exit (EXIT_FAILURE);
    }

    /* Create protection domain and register memory regions */
    endpoint->smb = cache_line_aligned_calloc (sizeof (*endpoint->smb));
    endpoint->device_pd = ibv_alloc_pd (endpoint->rdma_device);
    endpoint->mr = ibv_reg_mr (endpoint->device_pd, endpoint->smb, sizeof (*endpoint->smb),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    CHECK_ASSERT (endpoint->mr != NULL);

    /* Create a completion queue.
     * For the writer sized for the maximum write queue length.
     * For the reader not used, but has to be set a minimum length to avoid an error. */
    endpoint->cq = ibv_create_cq (endpoint->rdma_device, is_write_end ? (int) write_queue_length : 1, NULL, NULL, 0);
    CHECK_ASSERT (endpoint->cq != NULL);

    /* Create Queue-Pair */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = endpoint->cq;
    qp_init_attr.recv_cq = endpoint->cq;
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = is_write_end ? (write_queue_length * 2) /* data block + write index */ : 0;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = false;
    endpoint->qp = ibv_create_qp (endpoint->device_pd, &qp_init_attr);
    CHECK_ASSERT (endpoint->qp != NULL);

    /* Transition Queue-Pair to INIT state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = endpoint->port_num;
    qp_attr.qp_access_flags = is_write_end ? 0 : IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (endpoint->qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    CHECK_ASSERT (rc == 0);

    endpoint->initial_psn = get_random_psn ();
    rc = ibv_query_port (endpoint->rdma_device, endpoint->port_num, &endpoint->port_attributes);
    CHECK_ASSERT (rc == 0);
}


/**
 * @brief Connect a part of RDMA endpoints for the test, transitioning the Queue-Pairs to be able to send messages
 * @param[in/out] local_endpoint The local endpoint of the connection
 * @param[in] remote_endpoint The remote endpoint of the connection
 */
static void connect_rdma_endpoints (rdma_endpoint_t *const local_endpoint, const rdma_endpoint_t *const remote_endpoint)
{
    struct ibv_qp_attr qp_attr;
    int rc;

    /* Transition the queue-pair to the Ready-to-Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = local_endpoint->port_attributes.active_mtu;
    qp_attr.dest_qp_num = remote_endpoint->qp->qp_num;
    qp_attr.rq_psn = remote_endpoint->initial_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    qp_attr.ah_attr.is_global = local_endpoint->port_attributes.link_layer == IBV_LINK_LAYER_ETHERNET;
    if (qp_attr.ah_attr.is_global)
    {
        /* With an Ethernet link layer assume GID 0 is RoCEv1 */
        qp_attr.ah_attr.grh.hop_limit = 1;
        rc = ibv_query_gid (remote_endpoint->rdma_device, remote_endpoint->port_num, 0,
                &qp_attr.ah_attr.grh.dgid);
        CHECK_ASSERT (rc == 0);
        qp_attr.ah_attr.grh.sgid_index = 0;
    }
    qp_attr.ah_attr.dlid = remote_endpoint->port_attributes.lid;
    qp_attr.ah_attr.sl = 0;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = local_endpoint->port_num;
    rc = ibv_modify_qp (local_endpoint->qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_AV                 |
                        IBV_QP_PATH_MTU           |
                        IBV_QP_DEST_QPN           |
                        IBV_QP_RQ_PSN             |
                        IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    CHECK_ASSERT (rc == 0);

    /* Transition the queue-pair to the Ready-to-Send state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTS;
    qp_attr.sq_psn = local_endpoint->initial_psn;
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7; /* maximum */
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (local_endpoint->qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    CHECK_ASSERT (rc == 0);
}


/**
 * @brief Destroy the resources for a RDMA endpoint
 * @param[in/out] endpoint The RDMA endpoint to destroy
 */
static void destroy_rdma_endpoint (rdma_endpoint_t *const endpoint)
{
    int rc;

    rc = ibv_destroy_qp (endpoint->qp);
    CHECK_ASSERT (rc == 0);
    rc = ibv_destroy_cq (endpoint->cq);
    CHECK_ASSERT (rc == 0);
    rc = ibv_dereg_mr (endpoint->mr);
    CHECK_ASSERT (rc == 0);
    rc = ibv_dealloc_pd (endpoint->device_pd);
    CHECK_ASSERT (rc == 0);
    rc = ibv_close_device (endpoint->rdma_device);
    CHECK_ASSERT (rc == 0);
}


/**
 * @brief The entry point for the writer thread
 * @param[in/out] arg The context for the writer thread.
 * @return Not used
 */
static void *write_thread (void *const arg)
{
    write_thread_context_t *const context = arg;
    int num_completions;
    int rc;
    struct ibv_send_wr *bad_wr = NULL;

    context->write_index = 0;
    context->sequence_number = 1;

    /* Initialise the RDMA write queue.
     * The actual data block addresses are set during the test */
    context->write_queue = cache_line_aligned_calloc (sizeof (context->write_queue[0]) * context->write_queue_length);
    context->num_write_queue_entries_in_use = 0;
    context->max_write_queue_entries_in_use = 0;
    context->queue_index = 0;
    context->wcs = cache_line_aligned_calloc (sizeof (context->wcs[0]) * context->write_queue_length);
    for (uint32_t queue_index = 0; queue_index < context->write_queue_length; queue_index++)
    {
        write_queue_entry_t *const queue_entry = &context->write_queue[queue_index];

        queue_entry->data_block_sge.length = sizeof (data_block_t);
        queue_entry->data_block_sge.lkey = context->local_endpoint.mr->lkey;

        queue_entry->write_index_sge.addr = (uintptr_t) &context->local_endpoint.smb->write_index;
        queue_entry->write_index_sge.length = sizeof (context->local_endpoint.smb->write_index);
        queue_entry->write_index_sge.lkey = context->local_endpoint.mr->lkey;

        queue_entry->data_block_wr.sg_list = &queue_entry->data_block_sge;
        queue_entry->data_block_wr.num_sge = 1;
        queue_entry->data_block_wr.next = &queue_entry->write_index_wr;
        queue_entry->data_block_wr.opcode = IBV_WR_RDMA_WRITE;
        queue_entry->data_block_wr.send_flags = 0;
        queue_entry->data_block_wr.wr.rdma.rkey = context->remote_endpoint->mr->rkey;

        queue_entry->write_index_wr.sg_list = &queue_entry->write_index_sge;
        queue_entry->write_index_wr.num_sge = 1;
        queue_entry->write_index_wr.next = NULL;
        queue_entry->write_index_wr.opcode = IBV_WR_RDMA_WRITE;
        queue_entry->write_index_wr.send_flags = (context->send_index_inline ? IBV_SEND_INLINE : 0) | IBV_SEND_SIGNALED;
        queue_entry->write_index_wr.wr.rdma.remote_addr = (uintptr_t) &context->remote_endpoint->smb->write_index;
        queue_entry->write_index_wr.wr.rdma.rkey = context->remote_endpoint->mr->rkey;
    }

    /* Wait for reader to be ready. This isn't required to avoid data loss, but to maximum the chance of the reader sampling
     * an inconsistent sequence number during the test. */
    bool sampled_reader_ready = false;
    while (!sampled_reader_ready)
    {
        sampled_reader_ready = __atomic_load_n (&reader_ready, __ATOMIC_ACQUIRE);
    }

    for (uint32_t data_num = 0; data_num < context->test_num_data_writes; data_num++)
    {
        /* Wait until the reader has space for the next data block.
         * Since read_index == write_index indicates the queue is empty, the queue is not allowed to become completely full */
        const uint32_t next_write_index = (context->write_index + 1) % NUM_DATA_BLOCKS;
        do
        {
            context->sampled_read_index = __atomic_load_n (&global_read_index, __ATOMIC_ACQUIRE);
        } while (next_write_index == context->sampled_read_index);

        /* Process completions, blocking if the all transfers are in use */
        do
        {
            num_completions = ibv_poll_cq (context->local_endpoint.cq, context->write_queue_length, context->wcs);
            CHECK_ASSERT ((num_completions >= 0) && (num_completions <= context->num_write_queue_entries_in_use));
            context->num_write_queue_entries_in_use -= num_completions;
        } while (context->num_write_queue_entries_in_use == context->write_queue_length);

        /* Queue transfer for the next data block */
        data_block_t *const data_block = &context->local_endpoint.smb->data_blocks[context->write_index];
        write_queue_entry_t *const queue_entry = &context->write_queue[context->queue_index];

        data_block->sequence_number = context->sequence_number;
        queue_entry->data_block_sge.addr = (uintptr_t) data_block;
        queue_entry->data_block_wr.wr.rdma.remote_addr = (uintptr_t)
                &context->remote_endpoint->smb->data_blocks[context->write_index];
        context->write_index = (context->write_index + 1) % NUM_DATA_BLOCKS;
        __atomic_store_n (&context->local_endpoint.smb->write_index, context->write_index, __ATOMIC_RELEASE);
        rc = ibv_post_send (context->local_endpoint.qp, &queue_entry->data_block_wr, &bad_wr);
        CHECK_ASSERT (rc == 0);

        context->sequence_number++;
        context->num_write_queue_entries_in_use++;
        if (context->num_write_queue_entries_in_use > context->max_write_queue_entries_in_use)
        {
            context->max_write_queue_entries_in_use = context->num_write_queue_entries_in_use;
        }
        context->queue_index = (context->queue_index + 1) % context->write_queue_length;
    }

    return NULL;
}


/**
 * @brief The entry point for the reader thread
 * @param[in/out] arg The context for the reader thread.
 * @return Not used
 */
static void *read_thread (void *const arg)
{
    read_thread_context_t *const context = arg;
    uint32_t initial_sampled_sequence_number;

    context->unexpected_sequence_numbers =
            cache_line_aligned_calloc (sizeof (context->unexpected_sequence_numbers[0]) * context->test_num_data_writes);
    context->num_unexpected_sequence_numbers = 0;
    context->read_index = 0;
    context->expected_sequence_number = 1;
    __atomic_store_n (&reader_ready, true, __ATOMIC_RELEASE);

    for (uint32_t data_num = 0; data_num < context->test_num_data_writes; data_num++)
    {
        const uint32_t *const sequence_number = &context->local_endpoint.smb->data_blocks[context->read_index].sequence_number;

        /* Wait until the write index changes to indicate the next data block should be available */
        do
        {
            context->sampled_write_index = __atomic_load_n (&context->local_endpoint.smb->write_index, __ATOMIC_ACQUIRE);
        } while (context->sampled_write_index == context->read_index);

        initial_sampled_sequence_number = __atomic_load_n (sequence_number, __ATOMIC_ACQUIRE);
        if (initial_sampled_sequence_number != context->expected_sequence_number)
        {
            /* If don't sample the initial expected sequence number for the data block, poll for the expected sequence number */
            read_unexpected_sequence_number_t *const unexpected_sequence_number =
                    &context->unexpected_sequence_numbers[context->num_unexpected_sequence_numbers];

            while (__atomic_load_n (sequence_number, __ATOMIC_ACQUIRE) != context->expected_sequence_number)
            {
                unexpected_sequence_number->total_polls++;
            }
            unexpected_sequence_number->data_num = data_num;
            unexpected_sequence_number->expected_sequence_number = context->expected_sequence_number;
            context->num_unexpected_sequence_numbers++;
        }

        /* Advance to the next data block */
        context->expected_sequence_number++;
        context->read_index = (context->read_index + 1) % NUM_DATA_BLOCKS;
        __atomic_store_n (&global_read_index, context->read_index, __ATOMIC_RELEASE);
    }

    return NULL;
}


int main (int argc, char *argv[])
{
    int num_ibv_devices = 0;
    struct ibv_device **device_list;
    int rc;

    /* Extract fixed sequence command line arguments */
    if ((argc < 8) || (argc > 9))
    {
        fprintf (stderr, "Usage: %s <write_dev> <write_port> <read_dev> <read_port> <send_index_inline> <write_queue_length> <test_num_data_writes> [<csv_results_filename>]\n", argv[0]);
        exit (EXIT_FAILURE);
    }
    const char *const write_dev = argv[1];
    const uint8_t write_port = (uint8_t) atoi (argv[2]);
    const char *const read_dev = argv[3];
    const uint8_t read_port = (uint8_t) atoi (argv[4]);
    const bool send_index_inline = atoi (argv[5]) != 0;
    const uint32_t write_queue_length = (uint32_t) atoi (argv[6]);
    const uint32_t test_num_data_writes = (uint32_t) atoi (argv[7]);
    const char *const csv_results_filename = (argc >= 0) ? argv[8] : NULL;

    write_thread_context_t *const write_context = cache_line_aligned_calloc (sizeof (*write_context));
    read_thread_context_t *const read_context = cache_line_aligned_calloc (sizeof (*read_context));

    write_context->remote_endpoint = &read_context->local_endpoint;
    write_context->send_index_inline = send_index_inline;
    write_context->write_queue_length = write_queue_length;
    write_context->test_num_data_writes = test_num_data_writes;

    read_context->test_num_data_writes = test_num_data_writes;

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    CHECK_ASSERT (rc == 0);

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    /* Find all Infiniband devices */
    device_list = ibv_get_device_list (&num_ibv_devices);

    /* Initialise RDMA endpoints and connect them */
    open_rdma_endpoint (num_ibv_devices, device_list, write_dev, write_port, &write_context->local_endpoint,
            true, write_queue_length);
    open_rdma_endpoint (num_ibv_devices, device_list, read_dev, read_port, &read_context->local_endpoint,
            false, 0);
    connect_rdma_endpoints (&write_context->local_endpoint, &read_context->local_endpoint);
    connect_rdma_endpoints (&read_context->local_endpoint, &write_context->local_endpoint);

    /* Create the write and read threads with default attributes */
    pthread_t write_thread_id;
    pthread_t read_thread_id;
    rc = pthread_create (&write_thread_id, NULL, write_thread, write_context);
    CHECK_ASSERT (rc == 0);
    rc = pthread_create (&read_thread_id, NULL, read_thread, read_context);
    CHECK_ASSERT (rc == 0);

    /* Wait for the threads to exit */
    rc = pthread_join (write_thread_id, NULL);
    CHECK_ASSERT (rc == 0);
    rc = pthread_join (read_thread_id, NULL);
    CHECK_ASSERT (rc == 0);

    /* Display summary results on standard out */
    printf ("Write: max_write_queue_entries_in_use=%" PRIu32 "\n", write_context->max_write_queue_entries_in_use);
    printf ("Read: num_unexpected_sequence_numbers=%" PRIu32 "\n", read_context->num_unexpected_sequence_numbers);

    if (csv_results_filename != NULL)
    {
        /* Create a CSV file which records when the reader saw unexpected sequence numbers */
        FILE *const csv_results = fopen (csv_results_filename, "w");

        if (csv_results != NULL)
        {
            fprintf (csv_results, "Block number,Expected sequence number,Total polls\n");
            for (uint32_t sequence_index = 0; sequence_index < read_context->num_unexpected_sequence_numbers; sequence_index++)
            {
                const read_unexpected_sequence_number_t *const unexpected_sequence_number =
                        &read_context->unexpected_sequence_numbers[sequence_index];

                fprintf (csv_results, "%" PRIu32 ",%" PRIu32 ",%" PRIu32 "\n",
                        unexpected_sequence_number->data_num,
                        unexpected_sequence_number->expected_sequence_number,
                        unexpected_sequence_number->total_polls);
            }
            (void) fclose (csv_results);
        }
        else
        {
            printf ("Failed to create %s\n", csv_results_filename);
        }
    }

    /* Free resources */
    destroy_rdma_endpoint (&write_context->local_endpoint);
    destroy_rdma_endpoint (&read_context->local_endpoint);
    ibv_free_device_list (device_list);

    return EXIT_SUCCESS;
}
