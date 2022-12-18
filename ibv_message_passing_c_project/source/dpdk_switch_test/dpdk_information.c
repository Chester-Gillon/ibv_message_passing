/*
 * @file dpdk_information.c
 * @date 10 Dec 2022
 * @author Chester Gillon
 * @brief Program to test displaying information from DPDK about Ethernet devices
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include <net/if.h>

#include <rte_eal.h>
#include <rte_debug.h>
#include <rte_ethdev.h>

static void display_usage (const char *const prgname)
{
    printf ("This program doesn't use any arguments over and above those used by the DPDK EAL\n");
    printf ("prgname = %s\n\n", prgname);
}

int main (int argc, char *argv[])
{
    int rc;
    uint16_t port_id;
    struct rte_eth_dev_info dev_info;
    struct rte_eth_link link;
    struct rte_eth_fc_conf fc_conf;
    char if_name[IF_NAMESIZE];
    uint64_t offload;
    struct rte_eth_conf port_conf;
    uint16_t nb_rxd;
    uint16_t nb_txd;
    struct rte_mempool *mbuf_pool;
    char mbuf_pool_name[64];
    struct rte_eth_rxq_info rx_qinfo;
    struct rte_eth_txq_info tx_qinfo;

    /* Only need a single queue per device to be able to initialise the device to determine
     * the available fast-path operations. */
    const uint16_t num_rx_queues = 1;
    const uint16_t num_tx_queues = 1;
    const uint16_t queue_id = 0;

    /* For testing display of application usage, i.e. other than the description of the EAL options */
    (void) rte_set_application_usage_hook (display_usage);

    rc = rte_eal_init (argc, argv);
    if (rc < 0)
    {
        printf ("rte_eal_init() failed : %s\n", strerror (-rc));
        (void) rte_eal_cleanup();
        exit (EXIT_FAILURE);
    }

    /* Display the non EAL arguments. Always includes the program name as the first */
    printf ("Non EAL arguments:");
    for (int arg_index = rc; arg_index < argc; arg_index++)
    {
        printf (" %s", argv[arg_index]);
    }
    printf ("\n");

    /* Display the number of lcores which can be set by the -l option parsed by rte_eal_init().
     * Defaults to all cores, when the -l option isn't used. */
    printf ("rte_lcore_count = %u\n", rte_lcore_count());

    /* Display the IOVA mode */
    printf ("rte_eal_iova_mode : ");
    switch (rte_eal_iova_mode ())
    {
    case RTE_IOVA_DC:
        printf ("Don't care mode\n");
        break;

    case RTE_IOVA_PA:
        printf ("DMA using physical address\n");
        break;

    case RTE_IOVA_VA:
        printf ("DMA using virtual address\n");
        break;

    default:
        printf ("???\n");
        break;
    }

    /* Display counts of available devices */
    printf ("rte_eth_dev_count_total = %" PRIu16 "\n", rte_eth_dev_count_total ());
    printf ("rte_eth_dev_count_avail = %" PRIu16 "\n", rte_eth_dev_count_avail ());

    RTE_ETH_FOREACH_DEV (port_id)
    {
        const struct rte_eth_fp_ops *const ops = &rte_eth_fp_ops[port_id];
        rc = rte_eth_dev_info_get (port_id, &dev_info);
        if (rc != 0)
        {
            rte_exit(EXIT_FAILURE, "Error during getting device (port %u) info: %s\n", port_id, strerror(-rc));
        }
        printf ("\nport_id=%" PRIu16 " driver_name=%s\n", port_id, dev_info.driver_name);
        printf ("  device.name=%s\n", dev_info.device->name);

        printf ("  if_index=%u", dev_info.if_index);
        if (dev_info.if_index != 0)
        {
            if_indextoname (dev_info.if_index, if_name);
            printf ("  if_name=%s\n", if_name);
        }
        else
        {
            printf ("\n");
        }

        /* Display the offloading capabilities in the same layout as the testpmd utility.
         * For per-port (aka per-device) shows only capabilities which are *not* available per queue.
         */
        printf ("  Rx Offloading Capabilities:\n");
        printf ("    Per Queue :");
        for (offload = 1; offload != 0; offload <<= 1)
        {
            if ((dev_info.rx_queue_offload_capa & offload) != 0)
            {
                printf (" %s", rte_eth_dev_rx_offload_name (offload));
            }
        }
        printf ("\n");

        printf ("    Per Port  :");
        for (offload = 1; offload != 0; offload <<= 1)
        {
            if (((dev_info.rx_queue_offload_capa & offload) == 0) && ((dev_info.rx_offload_capa & offload) != 0))
            {
                printf (" %s", rte_eth_dev_rx_offload_name (offload));
            }
        }
        printf ("\n");

        printf ("  Tx Offloading Capabilities:\n");
        printf ("    Per Queue :");
        for (offload = 1; offload != 0; offload <<= 1)
        {
            if ((dev_info.tx_queue_offload_capa & offload) != 0)
            {
                printf (" %s", rte_eth_dev_tx_offload_name (offload));
            }
        }
        printf ("\n");
        printf ("    Per Port  :");
        for (offload = 1; offload != 0; offload <<= 1)
        {
            if (((dev_info.tx_queue_offload_capa & offload) == 0) && ((dev_info.tx_offload_capa & offload) != 0))
            {
                printf (" %s", rte_eth_dev_tx_offload_name (offload));
            }
        }
        printf ("\n");

        /* Configure device using defaults */
        memset (&port_conf, 0, sizeof (port_conf));
        rc = rte_eth_dev_configure (port_id, num_rx_queues, num_tx_queues, &port_conf);
        if (rc != 0)
        {
            printf ("rte_eth_dev_configure() failed : %s\n", strerror (-rc));
        }

        /* See what minimum number of descriptors is converted to */
        nb_rxd = 1;
        nb_txd = 1;
        rc = rte_eth_dev_adjust_nb_rx_tx_desc (port_id, &nb_rxd, &nb_txd);
        if (rc == 0)
        {
            printf ("  adjusted nb_rxd=%" PRIu16 " nb_txd=%" PRIu16 "\n", nb_rxd, nb_txd);
        }
        else
        {
            printf ("rte_eth_dev_adjust_nb_rx_tx_desc() failed : %s\n", strerror (-rc));
        }

        /* Create a pool with the number of descriptors, which are allocated when rte_eth_dev_start() is called */
        snprintf (mbuf_pool_name, sizeof (mbuf_pool_name), "MBUF_POOL_%" PRIu16, port_id);
        mbuf_pool = rte_pktmbuf_pool_create (mbuf_pool_name,
                                             nb_rxd,
                                             0,
                                             0,
                                             RTE_MBUF_DEFAULT_BUF_SIZE,
                                             rte_socket_id ());
        if (mbuf_pool == NULL)
        {
            printf ("rte_pktmbuf_pool_create() failed\n");
            (void) rte_eal_cleanup();
            exit (EXIT_FAILURE);
        }

        /* Allocate and setup 1 RX queue for the port */
        rc = rte_eth_rx_queue_setup (port_id, queue_id, nb_rxd, rte_eth_dev_socket_id (port_id), NULL, mbuf_pool);
        if (rc == 0)
        {
            rc = rte_eth_rx_queue_info_get (port_id, queue_id, &rx_qinfo);
            if (rc == 0)
            {
                printf ("  rx_qinfo nb_desc=%" PRIu16 " rx_buf_size=%" PRIu16 "\n",
                        rx_qinfo.nb_desc, rx_qinfo.rx_buf_size);
            }
            else
            {
                printf ("rte_eth_rx_queue_info_get() failed : %s\n", strerror (-rc));
            }
        }
        else
        {
            printf ("rte_eth_rx_queue_setup() failed : %s\n", strerror (-rc));
        }

        /* Allocate and setup 1 TX queue for the port */
        rc = rte_eth_tx_queue_setup (port_id, queue_id, nb_txd, rte_eth_dev_socket_id (port_id), NULL);
        if (rc == 0)
        {
            rc = rte_eth_tx_queue_info_get (port_id, queue_id, &tx_qinfo);
            if (rc == 0)
            {
                printf ("  tx_qinfo nb_desc=%" PRIu16 " pthresh=%" PRIu8 " hthresh=%" PRIu8 " wthresh=%" PRIu8 " tx_rs_thresh=%" PRIu16 " tx_free_thresh=%" PRIu16 "\n",
                        tx_qinfo.nb_desc,
                        tx_qinfo.conf.tx_thresh.pthresh, tx_qinfo.conf.tx_thresh.hthresh, tx_qinfo.conf.tx_thresh.wthresh,
                        tx_qinfo.conf.tx_rs_thresh, tx_qinfo.conf.tx_free_thresh);
            }
            else
            {
                printf ("rte_eth_tx_queue_info_get() failed : %s\n", strerror (-rc));
            }
        }
        else
        {
            printf ("rte_eth_tx_queue_setup() failed : %s\n", strerror (-rc));
        }

        /* Start the Ethernet port */
        rc = rte_eth_dev_start (port_id);
        if (rc != 0)
        {
            printf ("rte_eth_dev_start() failed : %s\n", strerror (-rc));
        }

        rc = rte_eth_link_get (port_id, &link);
        if (rc == 0)
        {
            printf ("  link_speed=%" PRIu32 " Mbps  link_duplex=%s  link_autoneg=%s  link_status=%s\n",
                    (link.link_speed == RTE_ETH_SPEED_NUM_UNKNOWN) ? 0 : link.link_speed,
                    (link.link_duplex == RTE_ETH_LINK_FULL_DUPLEX) ? "Full" : "Half",
                    (link.link_autoneg == RTE_ETH_LINK_AUTONEG) ? "Autonegotiated" : "Fixed",
                    (link.link_status == RTE_ETH_LINK_UP) ? "Up" : "Down");
        }
        else
        {
            printf ("rte_eth_link_get() failed : %s\n", strerror (-rc));
        }

        /* Display flow control settings */
        rc = rte_eth_dev_flow_ctrl_get (port_id, &fc_conf);
        if (rc == 0)
        {
            printf ("  Flow control mode : ");
            switch (fc_conf.mode)
            {
            case RTE_ETH_FC_NONE:
                printf ("Disable flow control\n");
                break;

            case RTE_ETH_FC_RX_PAUSE:
                printf ("Rx pause frame, enable flowctrl on Tx side\n");
                break;

            case RTE_ETH_FC_TX_PAUSE:
                printf ("Tx pause frame, enable flowctrl on Rx side\n");
                break;

            case RTE_ETH_FC_FULL:
                printf ("Enable flow control on both side\n");
                break;
            default:
                printf ("???\n");
                break;
            }
            printf ("  Flow control high_water=%" PRIu32 " low_water=%" PRIu32 " pause_time=%" PRIu16 " send_xon=%" PRIu16 " mac_ctrl_frame_fwd=%" PRIu8 " autoneg=%" PRIu8 "\n",
                    fc_conf.high_water,
                    fc_conf.low_water,
                    fc_conf.pause_time,
                    fc_conf.send_xon,
                    fc_conf.mac_ctrl_frame_fwd,
                    fc_conf.autoneg);
        }
        else
        {
            printf ("rte_eth_dev_flow_ctrl_get() failed : %s\n", strerror (-rc));
        }

        /* Display descriptor limits */
        printf ("  rx_desc_lim nb_max=%" PRIu16 " nb_min=%" PRIu16 " nb_align=%" PRIu16 " nb_seg_max=%" PRIu16 " nb_mtu_seg_max=%" PRIu16 "\n",
                dev_info.rx_desc_lim.nb_max,
                dev_info.rx_desc_lim.nb_min,
                dev_info.rx_desc_lim.nb_align,
                dev_info.rx_desc_lim.nb_seg_max,
                dev_info.rx_desc_lim.nb_mtu_seg_max);
        printf ("  tx_desc_lim nb_max=%" PRIu16 " nb_min=%" PRIu16 " nb_align=%" PRIu16 " nb_seg_max=%" PRIu16 " nb_mtu_seg_max=%" PRIu16 "\n",
                dev_info.tx_desc_lim.nb_max,
                dev_info.tx_desc_lim.nb_min,
                dev_info.tx_desc_lim.nb_align,
                dev_info.tx_desc_lim.nb_seg_max,
                dev_info.tx_desc_lim.nb_mtu_seg_max);

        /* Display configured number of queues */
        printf ("  nb_rx_queues=%" PRIu16 " nb_tx_queues=%" PRIu16 "\n", dev_info.nb_rx_queues, dev_info.nb_tx_queues);

        /* Display parameter recommendations */
        printf ("  default_rxportconf burst_size=%" PRIu16 " ring_size=%" PRIu16 " nb_queues=%" PRIu16 "\n",
                dev_info.default_rxportconf.burst_size,
                dev_info.default_rxportconf.ring_size,
                dev_info.default_rxportconf.nb_queues);
        printf ("  default_txportconf burst_size=%" PRIu16 " ring_size=%" PRIu16 " nb_queues=%" PRIu16 "\n",
                dev_info.default_txportconf.burst_size,
                dev_info.default_txportconf.ring_size,
                dev_info.default_txportconf.nb_queues);

        /* Display fast-path function pointers */
        printf ("  rx_pkt_burst=%p rx_queue_count=%p rx_descriptor_status=%p\n",
                ops->rx_pkt_burst, ops->rx_queue_count, ops->rx_descriptor_status);
        printf ("  tx_pkt_burst=%p tx_pkt_prepare=%p tx_descriptor_status=%p\n",
                ops->tx_pkt_burst, ops->tx_pkt_prepare, ops->tx_descriptor_status);
    }

    /* Stop and Close all the ports */
    RTE_ETH_FOREACH_DEV (port_id)
    {
        rc = rte_eth_dev_stop (port_id);
        if (rc == 0)
        {
            rc = rte_eth_dev_close (port_id);
            if (rc != 0)
            {
                printf ("rte_eth_dev_close() failed : %s\n", strerror (-rc));
            }
        }
        else
        {
            printf ("rte_eth_dev_stop() failed : %s\n", strerror (-rc));
        }
    }

    /* clean up the EAL */
    rc = rte_eal_cleanup();
    if (rc != 0)
    {
        printf ("rte_eal_cleanup() failed\n");
        exit (EXIT_FAILURE);
    }

    return EXIT_SUCCESS;
}
