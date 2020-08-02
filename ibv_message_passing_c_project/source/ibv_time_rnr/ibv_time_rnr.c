/*
 * @file ibv_time_rnr.c
 * @date 2 Aug 2020
 * @author Chester Gillon
 * @brief Test to measure the effect of the receiver-not-ready timer
 * @details Repeats a test of posting recv after send while IBV_WR_SEND opcode for RC connection using all possible
 *          values of the rnr_timer in the queue-pair.
 *
 *          For comparison also measures the delay with posting recv before send.
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
#include <sys/mman.h>


/* Used for RMDA buffers. Short size to minimum overheads */
#define TEST_BUFFER_SIZE 4
static uint8_t tx_buffer[TEST_BUFFER_SIZE];
static uint8_t rx_buffer[TEST_BUFFER_SIZE];


/* Infiniband ports used on the Infiniband loopback device */
#define TX_PORT 1
#define RX_PORT 2


/* The context for the loopback device across all tests.
 * Excludes queue-pairs and completion-queues which are created for each test. */
typedef struct
{
    struct ibv_context *loopback_device;
    struct ibv_device_attr loopback_device_attributes;
    struct ibv_port_attr tx_port_attributes;
    struct ibv_port_attr rx_port_attributes;
    struct ibv_pd *device_pd;
    struct ibv_mr *tx_mr;
    struct ibv_mr *rx_mr;
    struct ibv_sge tx_test_message_sge;
    struct ibv_sge rx_test_message_sge;
    struct ibv_send_wr tx_test_message_wr;
    struct ibv_recv_wr rx_test_message_wr;
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
 * @brief Measure the effect of the receiver-not-ready time
 * @param[in] min_rnr_timer The timer enumeration value to select
 * @param[in] post_recv_before_send control the order of posts:
 *            true :  recv is posted before send, which should not cause receiver-not-ready
 *            false : recv is posted after send, which should cause receiver-not-ready
 * @return Returns the elapsed number of microsends for the single test transfer to complete
 */
static int64_t time_rnr (const uint8_t min_rnr_timer, const bool post_recv_before_send,
                         loopback_device_context_t *const device_context)
{
    const uint32_t tx_psn = get_random_psn ();
    const uint32_t rx_psn = get_random_psn ();
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    struct ibv_cq *cq;
    struct ibv_qp *tx_qp;
    struct ibv_qp *rx_qp;
    struct ibv_send_wr *tx_bad_wr = NULL;
    struct ibv_recv_wr *rx_bad_wr = NULL;
    struct ibv_wc wc;
    int rc;
    int num_completions;
    struct timespec start;
    struct timespec stop;

    /* Create completion queue, referenced for both transmit and receive queue-pairs */
    cq = ibv_create_cq (device_context->loopback_device, 2, NULL, NULL, 0);
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
    qp_attr.min_rnr_timer = min_rnr_timer;
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
    qp_attr.min_rnr_timer = min_rnr_timer;
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
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7; /* maximum */
    qp_attr.rnr_retry = 7; /* Infinite */
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
    qp_attr.timeout = 14;
    qp_attr.retry_cnt = 7; /* maximum */
    qp_attr.rnr_retry = 7; /* Infinite */
    qp_attr.max_rd_atomic = 0;
    rc = ibv_modify_qp (rx_qp, &qp_attr,
                        IBV_QP_STATE              |
                        IBV_QP_TIMEOUT            |
                        IBV_QP_RETRY_CNT          |
                        IBV_QP_RNR_RETRY          |
                        IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    CHECK_ASSERT (rc == 0);

    for (int iteration = 0; iteration < 2; iteration++)
    {
        /* Post send and receive work-requests, with the order specified according to the supplied argument */
        if (post_recv_before_send)
        {
            rc = ibv_post_recv (rx_qp, &device_context->rx_test_message_wr, &rx_bad_wr);
            CHECK_ASSERT (rc == 0);
        }

        rc = clock_gettime (CLOCK_MONOTONIC, &start);
        CHECK_ASSERT (rc == 0);
        rc = ibv_post_send (tx_qp, &device_context->tx_test_message_wr, &tx_bad_wr);
        CHECK_ASSERT (rc == 0);

        if (!post_recv_before_send)
        {
            /* @todo Perform a read-back of the receive queue-pair state as a way of producing sufficient delay for the
             *       send to run into the receiver-not-ready condition which this program is designed to test.
             *
             *       Without this additional "delay" even though the program order has ibv_post_send() before the
             *       call to ibv_post_recv() then the receiver-not-ready delay was not seen. */
            rc = ibv_query_qp (rx_qp, &qp_attr, IBV_QP_STATE, &qp_init_attr);
            CHECK_ASSERT (rc == 0);

            rc = ibv_post_recv (rx_qp, &device_context->rx_test_message_wr, &rx_bad_wr);
            CHECK_ASSERT (rc == 0);
        }

        /* Wait for the send and receive work-requests to complete */
        bool send_complete = false;
        bool recv_complete = false;
        while (!send_complete || !recv_complete)
        {
            num_completions = ibv_poll_cq (cq, 1, &wc);
            CHECK_ASSERT ((num_completions >= 0) && (num_completions <= 1));
            if (num_completions == 1)
            {
                CHECK_ASSERT (wc.status == IBV_WC_SUCCESS);
                switch (wc.opcode)
                {
                case IBV_WC_SEND:
                    send_complete = true;
                    break;

                case IBV_WC_RECV:
                    rc = clock_gettime (CLOCK_MONOTONIC, &stop);
                    CHECK_ASSERT (rc == 0);
                    recv_complete = true;
                    break;

                default:
                    CHECK_ASSERT (false);
                    break;
                }
            }
        }
    }

    /* Destroy the queue-pairs and completion queue */
    rc = ibv_destroy_qp (rx_qp);
    CHECK_ASSERT (rc == 0);
    rc = ibv_destroy_qp (tx_qp);
    CHECK_ASSERT (rc == 0);
    rc = ibv_destroy_cq (cq);
    CHECK_ASSERT (rc == 0);

    /* Return the elapsed time for the transfer */
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
    bool post_recv_before_send;
    const uint8_t num_min_rnr_timer_values = 32;
    int64_t recv_first_durations[num_min_rnr_timer_values];
    int64_t send_first_durations[num_min_rnr_timer_values];

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    CHECK_ASSERT (rc == 0);

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

    /* Set a work-request to send from the tx_buffer */
    device_context.tx_test_message_sge.addr = (uintptr_t) tx_buffer;
    device_context.tx_test_message_sge.length = sizeof (tx_buffer);
    device_context.tx_test_message_sge.lkey = device_context.tx_mr->lkey;

    device_context.tx_test_message_wr.sg_list = &device_context.tx_test_message_sge;
    device_context.tx_test_message_wr.num_sge = 1;
    device_context.tx_test_message_wr.next = NULL;
    device_context.tx_test_message_wr.opcode = IBV_WR_SEND;
    device_context.tx_test_message_wr.send_flags = 0;

    /* Set a work-request to receive into the rx_buffer */
    device_context.rx_test_message_sge.addr = (uintptr_t) rx_buffer;
    device_context.rx_test_message_sge.length = sizeof (rx_buffer);
    device_context.rx_test_message_sge.lkey = device_context.rx_mr->lkey;

    device_context.rx_test_message_wr.sg_list = &device_context.rx_test_message_sge;
    device_context.rx_test_message_wr.num_sge = 1;
    device_context.rx_test_message_wr.next = NULL;

    /* Test all the min_rnr_timer values */
    for (uint8_t min_rnr_timer = 0; min_rnr_timer < num_min_rnr_timer_values; min_rnr_timer++)
    {
        post_recv_before_send = true;
        recv_first_durations[min_rnr_timer] = time_rnr (min_rnr_timer, post_recv_before_send, &device_context);
        post_recv_before_send = false;
        send_first_durations[min_rnr_timer] = time_rnr (min_rnr_timer, post_recv_before_send, &device_context);
    }

    printf ("min_rnr_timer,duration post recv-before-send (us),duration post-recv-after-send (us)\n");
    for (uint8_t min_rnr_timer = 0; min_rnr_timer < num_min_rnr_timer_values; min_rnr_timer++)
    {
        printf ("%u,%ld,%ld\n", min_rnr_timer, recv_first_durations[min_rnr_timer], send_first_durations[min_rnr_timer]);
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
