/*
 * @file ibv_display_infiniband_counters_main.c
 * @date 7 Oct 2017
 * @author Chester Gillon
 * @brief Program which demonstrates programatically reading the counters for all Infiniband ports in a host
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <infiniband/verbs.h>
#include <infiniband/mad.h>

/**
 * @brief Display a 32-bit MAD field value
 * @param[in] mad_buf The buffer containing the field value
 * @param[in] field The enumerated field value to display
 * @param[in] field_name The name of the field value to display
 */
#define DISPLAY_32BIT_FIELD(mad_buf,field) display_32bit_field (mad_buf, field, #field)
static void display_32bit_field (uint8_t *const mad_buf, const enum MAD_FIELDS field, const char *field_name)
{
    uint32_t field_value = 0;

    mad_decode_field (mad_buf, field, &field_value);
    printf ("  %s = %u\n", field_name, field_value);
}

/**
 * @brief Display a 64-bit MAD field value
 * @param[in] mad_buf The buffer containing the field value
 * @param[in] field The enumerated field value to display
 * @param[in] field_name The name of the field value to display
 */
#define DISPLAY_64BIT_FIELD(mad_buf,field) display_64bit_field (mad_buf, field, #field)
static void display_64bit_field (uint8_t *const mad_buf, const enum MAD_FIELDS field, const char *field_name)
{
    uint64_t field_value = 0;

    mad_decode_field (mad_buf, field, &field_value);
    printf ("  %s = %lu\n", field_name, field_value);
}

/**
 * @brief Obtain and display the current counter values for one Infiniband port
 * @param[in] hca Name of the Infiniband device to display the counter values for
 * @param[in] port_num Which port on the Infiniband device to display the counter values for
 * @param[in] port_attributes The attributes for the Infiniband port, to obtain the port LID
 */
static void display_port_counters (char *const hca, const uint8_t port_num, const struct ibv_port_attr *const port_attributes)
{
    int mgmt_class = IB_PERFORMANCE_CLASS;
    struct ibmad_port *mad_port;
    ib_portid_t portid;
    uint8_t mad_buf[1024];
    const int timeout = 0;

    mad_port = mad_rpc_open_port (hca, port_num, &mgmt_class, 1);
    if (mad_port == NULL)
    {
        fprintf (stderr, "mad_rpc_open_port failed for %s port %u\n", hca, port_num);
        exit (EXIT_FAILURE);
    }

    memset (&portid, 0, sizeof (portid));
    portid.lid = port_attributes->lid;
    memset (mad_buf, 0, sizeof (mad_buf));
    if (pma_query_via (mad_buf, &portid, port_num, timeout, IB_GSI_PORT_COUNTERS_EXT, mad_port) == NULL)
    {
        fprintf (stderr, "pma_query_via failed for %s LID %u\n", hca, portid.lid);
        exit (EXIT_FAILURE);
    }

    printf ("Counters for device %s port %u LID %u\n", hca, port_num, portid.lid);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_BYTES_F);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_BYTES_F);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_PKTS_F);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_PKTS_F);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_UPKTS_F);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_UPKTS_F);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_MPKTS_F);
    DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_MPKTS_F);

    memset (mad_buf, 0, sizeof (mad_buf));
    if (pma_query_via (mad_buf, &portid, port_num, timeout, IB_GSI_PORT_COUNTERS, mad_port) == NULL)
    {
        fprintf (stderr, "pma_query_via failed for %s LID %u\n", hca, portid.lid);
        exit (EXIT_FAILURE);
    }
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_SYM_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_LINK_RECOVERS_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_LINK_DOWNED_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_RCV_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_PHYSRCV_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_SWITCH_REL_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_XMT_DISCARDS_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_XMTCONSTR_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_RCVCONSTR_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_LOCALINTEG_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_ERR_EXCESS_OVR_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_VL15_DROPPED_F);
    DISPLAY_32BIT_FIELD (mad_buf, IB_PC_XMT_WAIT_F);
    printf ("\n");

    mad_rpc_close_port (mad_port);
}

/**
 * @brief Obtain and display the current counter values for all ports on an Infiniband device
 * @param[in] device The Infiniband device to display the counter values for
 * @param[in] context The open context for device
 */
static void display_device_counters (struct ibv_device *const device, struct ibv_context *const context)
{
    char *const hca = strdup (ibv_get_device_name (device));
    int rc;
    struct ibv_device_attr device_attributes;
    struct ibv_port_attr port_attributes;
    uint8_t port_num;

    rc = ibv_query_device (context, &device_attributes);
    if (rc != 0)
    {
        perror ("ibv_query_device failed");
        exit (EXIT_FAILURE);
    }

    for (port_num = 1; port_num <= device_attributes.phys_port_cnt; port_num++)
    {
        rc = ibv_query_port (context, port_num, &port_attributes);
        if (rc != 0)
        {
            perror ("ibv_query_port failed");
            exit (EXIT_FAILURE);
        }
        display_port_counters (hca, port_num, &port_attributes);
    }

    free (hca);
}

int main (int argc, char *argv[])
{
    int num_ibv_devices;
    struct ibv_device **ibv_device_list;
    unsigned int device_index;
    int rc;

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

    /* Display the counters for all Infiniband devices */
    for (device_index = 0; device_index < num_ibv_devices; device_index++)
    {
        struct ibv_device *const device = ibv_device_list[device_index];
        struct ibv_context *const context = ibv_open_device (device);

        if (device == NULL)
        {
            fprintf (stderr, "ibv_open_device failed\n");
            exit (EXIT_FAILURE);
        }
        display_device_counters (device, context);
        rc = ibv_close_device (context);
        if (rc != 0)
        {
            fprintf (stderr, "ibv_close_device failed\n");
            exit (EXIT_SUCCESS);
        }
    }

    ibv_free_device_list (ibv_device_list);

    return EXIT_SUCCESS;
}
