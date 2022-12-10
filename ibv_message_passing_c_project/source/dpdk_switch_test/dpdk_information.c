/*
 * @file dpdk_information.c
 * @date 10 Dec 2022
 * @author Chester Gillon
 * @brief Program to test displaying information from DPDK about Ethernet devices
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include <net/if.h>

#include <rte_eal.h>
#include <rte_debug.h>
#include <rte_ethdev.h>

int main (int argc, char *argv[])
{
    int rc;
    uint16_t port_id;
    struct rte_eth_dev_info dev_info;
    struct rte_eth_link link;
    char if_name[IF_NAMESIZE];
    uint64_t offload;

    rc = rte_eal_init (argc, argv);
    if (rc < 0)
    {
        rte_panic("Cannot init EAL\n");
    }

    /* Display counts of available devices */
    printf ("rte_eth_dev_count_total = %" PRIu16 "\n", rte_eth_dev_count_total ());
    printf ("rte_eth_dev_count_avail = %" PRIu16 "\n", rte_eth_dev_count_avail ());

    RTE_ETH_FOREACH_DEV (port_id)
    {
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
         * For per-port (aka per-devidce) shows only capabilities which are *not* available per queue.
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
