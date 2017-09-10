/*
 * @file ibv_functional_loopback_test_main.c
 * @date 3 Sep 2017
 * @author Chester Gillon
 * @details Perform functional Infiniband message passing tests with a single threaded program,
 *          assuming a dual port Infiniband adapter with port 1 looped to port 2.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <infiniband/verbs.h>

#include "ibv_utils.h"
#include "ibv_functional_loopback_test_interface.h"

/** The list of all Infiniband devices */
static int num_ibv_devices;
static struct ibv_device **ibv_device_list;

/** The Infiniband device used for the loopback tests, which is the first device found */
struct ibv_context *ibv_loopback_device;
struct ibv_device_attr ibv_loopback_device_attributes;

/** The ports used on the Infiniband device used for the loopback tests */
struct ibv_port_attr ibv_loopback_port_attributes[NUM_TEST_PORTS+1];

/** The protection domain for the Infiniband device used for the loopback tests */
struct ibv_pd *ibv_loopback_device_pd;

/**
 * @brief Open the Infiniband device to be used for the loopback tests, including obtaining the port attributes
 */
static void open_infiniband_loopback_ports (void)
{
    int rc;
    int port_num;

    /* Find all Infiniband devices */
    ibv_device_list = ibv_get_device_list (&num_ibv_devices);
    if (ibv_device_list == NULL)
    {
        perror ("ibv_get_device_list failed");
        exit (EXIT_FAILURE);
    }
    if (num_ibv_devices == 0)
    {
        fprintf (stderr, "No Infiniband devices found\n");
        exit (EXIT_FAILURE);
    }

    /* Open the first device for use in the loopback tests */
    ibv_loopback_device = ibv_open_device (ibv_device_list[0]);
    if (ibv_loopback_device == NULL)
    {
        fprintf (stderr, "ibv_open_device failed\n");
        exit (EXIT_FAILURE);
    }

    /* Display the attributes of the loopback device */
    rc = ibv_query_device (ibv_loopback_device, &ibv_loopback_device_attributes);
    if (rc != 0)
    {
        perror ("ibv_query_device failed");
        exit (EXIT_FAILURE);
    }
    display_ibv_device_attributes (ibv_loopback_device, &ibv_loopback_device_attributes);

    /* Display the attributes of the ports used for the loopback tests */
    if (ibv_loopback_device_attributes.phys_port_cnt < NUM_TEST_PORTS)
    {
        fprintf (stderr, "Insufficient Infiniband ports on device for loopback test\n");
        exit (EXIT_FAILURE);
    }
    for (port_num = 1; port_num <= NUM_TEST_PORTS; port_num++)
    {
        rc = ibv_query_port (ibv_loopback_device, port_num, &ibv_loopback_port_attributes[port_num]);
        if (rc != 0)
        {
            perror ("ibv_query_port failed");
            exit (EXIT_FAILURE);
        }
    }
    printf ("Attributes of source port %u:\n", SOURCE_PORT_NUM);
    display_ibv_port_attributes (&ibv_loopback_port_attributes[SOURCE_PORT_NUM]);
    printf ("Attributes of destination port %u:\n", DESTINATION_PORT_NUM);
    display_ibv_port_attributes (&ibv_loopback_port_attributes[DESTINATION_PORT_NUM]);

    /* Create the single protection domain used for all tests */
    ibv_loopback_device_pd = ibv_alloc_pd (ibv_loopback_device);
    if (ibv_loopback_device_pd == NULL)
    {
        fprintf (stderr, "ibv_alloc_pd failed\n");
        exit (EXIT_SUCCESS);
    }
}

/**
 * @brief Close the Infiniband device used for the loopback tests
 */
static void close_ininiband_loopback_ports (void)
{
    int rc;

    rc = ibv_dealloc_pd (ibv_loopback_device_pd);
    if (rc != 0)
    {
        perror ("ibv_dealloc_pd failed");
        exit (EXIT_FAILURE);
    }

    rc = ibv_close_device (ibv_loopback_device);
    if (rc != 0)
    {
        fprintf (stderr, "ibv_close_device failed\n");
        exit (EXIT_SUCCESS);
    }
    ibv_free_device_list (ibv_device_list);
}

int main (int argc, char *argv[])
{
    int rc;

    /* Add protection against fork() being called */
    rc = ibv_fork_init ();
    if (rc != 0)
    {
       perror ("ibv_fork_init failed");
       exit (EXIT_FAILURE);
    }

    /* To allow generation of random Packet Sequence numbers */
    srand48 (getpid() * time(NULL));

    open_infiniband_loopback_ports ();
    test_sender_rdma_write_receiver_passive ();
    close_ininiband_loopback_ports ();

    return EXIT_FAILURE;
}
