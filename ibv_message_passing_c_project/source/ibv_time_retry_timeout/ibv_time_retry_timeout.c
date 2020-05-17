/*
 * @file ibv_time_retry_timeout.c
 * @date 12 May 2019
 * @author Chester Gillon
 * @brief Time the elapsed retry time for an Infiniband Reliable-Connected (RC) for a range of timeout and retry_cnt.
 * @details This is to determine if the retry time:
 *          a. Scales with the number of retries.
 *          b. If the Infiniband device imposes a lower limit on the timeout value.
 *
 *          A retry timeout is forced to occur by starting with a successful RDMA write between two queue-pairs
 *          using two ports on a local Infiniband device, but destroying the receive queue-pair and then
 *          attempting another RDMA write.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>
#include <time.h>

#include <infiniband/verbs.h>


/* Used for RMDA buffers */
#define TEST_BUFFER_SIZE 4096
static uint8_t tx_buffer[TEST_BUFFER_SIZE];
static uint8_t rx_buffer[TEST_BUFFER_SIZE];


/* Infiniband ports used on the Infiniband loopback device */
#define TX_PORT 1
#define RX_PORT 2


/* The context for the loopback device across all tests for different retry timer settings.
 * Excludes queue-pairs and completion-queues which are created for each test */
typedef struct
{
    struct ibv_context *loopback_device;
    struct ibv_device_attr loopback_device_attributes;
    struct ibv_port_attr tx_port_attributes;
    struct ibv_port_attr rx_port_attributes;
    struct ibv_pd *device_pd;
    struct ibv_mr *tx_mr;
    struct ibv_mr *rx_mr;
    struct ibv_sge test_message_sqe;
    struct ibv_send_wr test_message_wr;
} loopback_device_context_t;


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
 * @brief Obtain a pseudo-random 24-bit Packet Sequence Number
 * @return Returns the starting Packet Sequence Number
 */
static uint32_t get_random_psn (void)
{
    return lrand48 () & 0xffffff;
}


/**
 * @brief Measure the Infiniband reliable-connection (RC) elapsed retry timeout for a given set of parameters
 * @param[in] timeout The timeout value to use for the Queue-Pair.
 * @param[in] retry_cnt The retry count to use for the Queue-Pair.
 * @param[in] device_context The context for the Infiniband device to use for measuring the retry timeout.
 * @return The elapsed retry timeout in microseconds
 */
static int64_t time_retry_timeout (const uint8_t timeout, const uint8_t retry_cnt,
                                   loopback_device_context_t *const device_context)
{
    const uint32_t tx_psn = get_random_psn ();
    const uint32_t rx_psn = get_random_psn ();
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    struct ibv_cq *cq;
    struct ibv_qp *tx_qp;
    struct ibv_qp *rx_qp;
    struct ibv_send_wr *bad_wr = NULL;
    struct ibv_wc wc;
    int rc;
    int num_completions;
    struct timespec start;
    struct timespec stop;

    /* Create completion queue, referenced for both transmit and receive queue-pairs but only used for RDMA writes */
    cq = ibv_create_cq (device_context->loopback_device, 1, NULL, NULL, 0);
    CHECK_ASSERT (cq != NULL);

    /* Create queue-pairs */
    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    qp_init_attr.qp_context = NULL;
    qp_init_attr.send_cq = cq;
    qp_init_attr.recv_cq = cq;
    qp_init_attr.srq = NULL;
    qp_init_attr.cap.max_send_wr = 1;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    qp_init_attr.qp_type = IBV_QPT_RC;
    qp_init_attr.sq_sig_all = true;
    tx_qp = ibv_create_qp (device_context->device_pd, &qp_init_attr);
    CHECK_ASSERT (tx_qp != NULL);

    qp_init_attr.cap.max_send_wr = 0;
    qp_init_attr.cap.max_send_sge = 0;
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_recv_sge = 0;
    qp_init_attr.cap.max_inline_data = 0;
    rx_qp = ibv_create_qp (device_context->device_pd, &qp_init_attr);
    CHECK_ASSERT (rx_qp != NULL);

    /* Transition queue-pairs to the INIT state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = TX_PORT;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (tx_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    CHECK_ASSERT (rc == 0);

    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = 0;
    qp_attr.port_num = RX_PORT;
    qp_attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE;
    rc = ibv_modify_qp (rx_qp, &qp_attr,
                        IBV_QP_STATE      |
                        IBV_QP_PKEY_INDEX |
                        IBV_QP_PORT       |
                        IBV_QP_ACCESS_FLAGS);
    CHECK_ASSERT (rc == 0);

    /* Transition the queue-pairs to the Ready-to-Receive state */
    memset (&qp_attr, 0, sizeof (qp_attr));
    qp_attr.qp_state = IBV_QPS_RTR;
    qp_attr.path_mtu = device_context->tx_port_attributes.active_mtu;
    qp_attr.dest_qp_num = rx_qp->qp_num;
    qp_attr.rq_psn = rx_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = device_context->rx_port_attributes.lid;
    qp_attr.ah_attr.sl = 0;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = TX_PORT;
    rc = ibv_modify_qp (tx_qp, &qp_attr,
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
    qp_attr.path_mtu = device_context->rx_port_attributes.active_mtu;
    qp_attr.dest_qp_num = tx_qp->qp_num;
    qp_attr.rq_psn = tx_psn;
    qp_attr.max_dest_rd_atomic = 0;
    qp_attr.min_rnr_timer = 0;
    qp_attr.ah_attr.is_global = false;
    qp_attr.ah_attr.dlid = device_context->tx_port_attributes.lid;
    qp_attr.ah_attr.sl = 0;
    qp_attr.ah_attr.src_path_bits = 0;
    qp_attr.ah_attr.port_num = RX_PORT;
    rc = ibv_modify_qp (rx_qp, &qp_attr,
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
    rc = ibv_modify_qp (tx_qp, &qp_attr,
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
    rc = ibv_modify_qp (rx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    CHECK_ASSERT (rc == 0);

    /* Send one message from the transmitter to receiver, which should complete without error to indicate the
     * queue-pairs have been successfully initialised */
    rc = ibv_post_send (tx_qp, &device_context->test_message_wr, &bad_wr);
    CHECK_ASSERT (rc == 0);
    do
    {
        num_completions = ibv_poll_cq (cq, 1, &wc);
    } while (num_completions == 0);
    CHECK_ASSERT (num_completions == 1);
    CHECK_ASSERT (wc.status == IBV_WC_SUCCESS);

    /* Destroy the receive queue-pair */
    rc = ibv_destroy_qp (rx_qp);
    CHECK_ASSERT (rc == 0);

    /* Attempt to send another message from the transmitter to receiver, which should fail with a retry timeout due to
     * the receive queue-pair having been destroyed. */
    rc = clock_gettime (CLOCK_MONOTONIC, &start);
    CHECK_ASSERT (rc == 0);
    rc = ibv_post_send (tx_qp, &device_context->test_message_wr, &bad_wr);
    CHECK_ASSERT (rc == 0);
    do
    {
        num_completions = ibv_poll_cq (cq, 1, &wc);
    } while (num_completions == 0);
    rc = clock_gettime (CLOCK_MONOTONIC, &stop);
    CHECK_ASSERT (rc == 0);
    CHECK_ASSERT (num_completions == 1);
    CHECK_ASSERT (wc.status == IBV_WC_RETRY_EXC_ERR);

    /* Destroy the transmit queue-pair and completion queue */
    rc = ibv_destroy_qp (tx_qp);
    CHECK_ASSERT (rc == 0);
    rc = ibv_destroy_cq (cq);
    CHECK_ASSERT (rc == 0);

    const int64_t nsecs_per_sec = 1000000000;
    const int64_t usecs_per_nsec = 1000;
    const int64_t start_ns = (start.tv_sec * nsecs_per_sec) + start.tv_nsec;
    const int64_t stop_ns =  ( stop.tv_sec * nsecs_per_sec) + stop.tv_nsec;
    return (stop_ns - start_ns) / usecs_per_nsec;
}


int main (int argc, char *argv[])
{
    int num_ibv_devices = 0;
    struct ibv_device **device_list;
    loopback_device_context_t device_context;
    int rc;
    uint8_t timeout;
    uint8_t retry_cnt;

    /* Find all Infiniband devices */
    device_list = ibv_get_device_list (&num_ibv_devices);
    check_assert (num_ibv_devices > 0, "No Infiniband devices found");

    /* Open the first device, assumed to have two ports for external loopback */
    memset (&device_context, 0, sizeof (device_context));
    device_context.loopback_device = ibv_open_device (device_list[0]);
    CHECK_ASSERT (device_context.loopback_device != NULL);
    rc = ibv_query_device (device_context.loopback_device, &device_context.loopback_device_attributes);
    CHECK_ASSERT (rc == 0);
    CHECK_ASSERT (device_context.loopback_device_attributes.phys_port_cnt >= 2);
    rc = ibv_query_port (device_context.loopback_device, TX_PORT, &device_context.tx_port_attributes);
    CHECK_ASSERT (rc == 0);
    rc = ibv_query_port (device_context.loopback_device, RX_PORT, &device_context.rx_port_attributes);
    CHECK_ASSERT (rc == 0);

    /* Create protection domain and register memory regions */
    device_context.device_pd = ibv_alloc_pd (device_context.loopback_device);
    CHECK_ASSERT (device_context.device_pd != NULL);
    device_context.tx_mr = ibv_reg_mr (device_context.device_pd, tx_buffer, sizeof (tx_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    CHECK_ASSERT (device_context.tx_mr != NULL);
    device_context.rx_mr = ibv_reg_mr (device_context.device_pd, rx_buffer, sizeof (rx_buffer),
            IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
    CHECK_ASSERT (device_context.rx_mr != NULL);

    /* Set a work-request to transfer the tx_buffer to rx_buffer */
    device_context.test_message_sqe.addr = (uintptr_t) tx_buffer;
    device_context.test_message_sqe.length = sizeof (tx_buffer);
    device_context.test_message_sqe.lkey = device_context.tx_mr->lkey;

    device_context.test_message_wr.sg_list = &device_context.test_message_sqe;
    device_context.test_message_wr.num_sge = 1;
    device_context.test_message_wr.next = NULL;
    device_context.test_message_wr.opcode = IBV_WR_RDMA_WRITE;
    device_context.test_message_wr.send_flags = 0;
    device_context.test_message_wr.wr.rdma.remote_addr = (uintptr_t) rx_buffer;
    device_context.test_message_wr.wr.rdma.rkey = device_context.rx_mr->rkey;

    /* Display the Infiniband device local_ca_ack_delay as might be related to the minimum timeout */
    printf ("%s (vendor_id=0x%" PRIx32 " vendor_part_id=0x%" PRIx32 ") local_ca_ack_delay=%u\n\n",
            device_context.loopback_device->device->dev_name,
            device_context.loopback_device_attributes.vendor_id,
            device_context.loopback_device_attributes.vendor_part_id,
            device_context.loopback_device_attributes.local_ca_ack_delay);

    /* Test all non-infinite timeout values, displaying the measured values to stdout */
    int64_t retry_time_us;
    printf ("timeout,retry_cnt,elapsed retry time (us)\n");
    for (timeout = 1; timeout <= 31; timeout++)
    {
        /* Try a subset of the 3-bit retry count values to see if the time scales with the number of retries */
        for (retry_cnt = 0; retry_cnt <= 2; retry_cnt++)
        {
            retry_time_us = time_retry_timeout (timeout, retry_cnt, &device_context);
            printf ("%u,%u,%" PRIi64 "\n", timeout, retry_cnt, retry_time_us);
        }
    }

    /* Free device resources */
    rc = ibv_dereg_mr (device_context.tx_mr);
    CHECK_ASSERT (rc == 0);
    rc = ibv_dereg_mr (device_context.rx_mr);
    CHECK_ASSERT (rc == 0);
    rc = ibv_dealloc_pd (device_context.device_pd);
    CHECK_ASSERT (rc == 0);
    rc = ibv_close_device (device_context.loopback_device);
    CHECK_ASSERT (rc == 0);
    ibv_free_device_list (device_list);

    return EXIT_SUCCESS;
}
