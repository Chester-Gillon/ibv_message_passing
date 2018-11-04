/*
 * @file ibv_display_infiniband_counters_main.c
 * @date 7 Oct 2017
 * @author Chester Gillon
 * @brief Program which demonstrates programatically reading the counters for all Infiniband ports in the fabric
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#include <infiniband/verbs.h>
#include <infiniband/mad.h>
#include <infiniband/iba/ib_types.h>
#include <infiniband/ibnetdisc.h>


/** Defines the fabric which has been discovered from one port local to the calling host */
typedef struct
{
    /** The Infiniband device name of the local HCA */
    char *local_hca;
    /** Which port of the local_hca the fabric has been discovered on */
    uint8_t local_port_num;
    /** Used to sent MAD requests to query the performance counters of the ports in discovered */
    struct ibmad_port *mad_port;
    /** The fabric which has been discovered from the local port */
    ibnd_fabric_t *discovered;
} fabric_from_local_port_t;


/** Define one port in the fabric for which performance counters can be read */
typedef struct
{
    /** Points at the parent structure from which this port was discovered, to be able to use the mad_port to read the
     *  performance counters for the port */
    fabric_from_local_port_t *fabric_from_local_port;
    /** Defines the port for which the performance counters may be read */
    ibnd_port_t *fabric_port;
} unique_port_for_counters_t;


/** Used to discover the fabric which is visible from all local ports on the local host */
#define MAX_LOCAL_PORTS 16
typedef struct
{
    /** The number of Infiniband devices on the local host */
    int num_ibv_devices;
    /** The list of Infiniband devices on the local host */
    struct ibv_device **ibv_device_list;
    /** The total number of ports on the local host */
    uint32_t num_local_ports;
    /** The fabric which has been discovered from each port on the local host.
     *  If more than port on the local host is connected to the same Infiniband subnet then discovered hosts, switches and ports
     *  may be present in more than one element of the array. */
    fabric_from_local_port_t fabric_from_local_ports[MAX_LOCAL_PORTS];
    /** The total number of unique ports from which performance counters may be read */
    uint32_t num_unique_ports;
    /** The array of unique ports from which performance counters may be read.
     *  Switch zero ports which don't support performance counters are excluded from this list. */
    unique_port_for_counters_t *unique_ports;
} discovered_fabric_t;


/**
 * @brief Abort the program if an assertion fails, after displaying a message
 * @param[in] assertion Should be true to allow the program to continue.
 * @param[in] format printf style format string for error message.
 * @param[in] ... printf arguments
 */
static void check_assert (const bool assertion, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
static void check_assert (const bool assertion, const char *format, ...)
{
    if (!assertion)
    {
        va_list args;

        va_start (args, format);
        vfprintf (stderr, format, args);
        va_end (args);
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }
}


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


/*
 * @brief Discover the Infiniband fabric by attempting to probe from all ports on all local Infiniband devices.
 * @param[out] fabric The discovered fabric.
 */
static void discover_fabric (discovered_fabric_t *const fabric)
{
    int device_index;
    int rc;
    uint8_t port_num;

    /* Find all local Infiniband devices */
    memset (fabric, 0, sizeof (*fabric));
    fabric->ibv_device_list = ibv_get_device_list (&fabric->num_ibv_devices);
    check_assert (fabric->ibv_device_list != NULL, "ibv_get_device_list failed");
    check_assert (fabric->num_ibv_devices > 0, "No Infiniband devices found");

    fabric->num_local_ports = 0;
    for (device_index = 0; device_index < fabric->num_ibv_devices; device_index++)
    {
        struct ibv_device *const device = fabric->ibv_device_list[device_index];
        struct ibv_context *const context = ibv_open_device (device);
        struct ibv_device_attr device_attributes;

        check_assert (context != NULL, "ibv_open_device failed");
        rc = ibv_query_device (context, &device_attributes);
        check_assert (rc == 0, "ibv_query_device failed");

        for (port_num = 1; port_num <= device_attributes.phys_port_cnt; port_num++)
        {
            fabric_from_local_port_t *const fabric_from_local_port = &fabric->fabric_from_local_ports[fabric->num_local_ports];
            int mgmt_classes[3] = {IB_SMI_CLASS, IB_SMI_DIRECT_CLASS, IB_PERFORMANCE_CLASS};
            ibnd_config_t config = {0};

            check_assert (fabric->num_local_ports < MAX_LOCAL_PORTS, "Too many local ports");
            fabric_from_local_port->local_hca = strdup (ibv_get_device_name (device));
            fabric_from_local_port->local_port_num = port_num;
            fabric_from_local_port->mad_port =
                    mad_rpc_open_port (fabric_from_local_port->local_hca, fabric_from_local_port->local_port_num, mgmt_classes, 3);
            check_assert (fabric_from_local_port->mad_port != NULL, "mad_rpc_open_port failed for %s port %u",
                    fabric_from_local_port->local_hca, fabric_from_local_port->local_port_num);

            fabric_from_local_port->discovered =
                    ibnd_discover_fabric (fabric_from_local_port->local_hca, fabric_from_local_port->local_port_num, NULL, &config);

            fabric->num_local_ports++;
        }

        rc = ibv_close_device (context);
        check_assert (rc == 0, "ibv_close_device failed");
    }
}


/**
 * @brief Release all the resources for the discovered fabric
 * @param[in/out] fabric The discovered fabric to destroy.
 */
static void destroy_fabric (discovered_fabric_t *const fabric)
{
    uint32_t local_port_index;

    free (fabric->unique_ports);
    for (local_port_index = 0; local_port_index < fabric->num_local_ports; local_port_index++)
    {
        fabric_from_local_port_t *const fabric_from_local_port = &fabric->fabric_from_local_ports[local_port_index];

        ibnd_destroy_fabric (fabric_from_local_port->discovered);
        mad_rpc_close_port (fabric_from_local_port->mad_port);
        free (fabric_from_local_port->local_hca);
    }
    ibv_free_device_list (fabric->ibv_device_list);
}


/**
 * @brief Find the list of unique ports in the fabric for which performance counters can be read.
 * @details Unique ports are determined from their GUID and port number.
 * @param[in/out] fabric The fabric to find the list of unique ports for
 */
static void find_unique_ports (discovered_fabric_t *const fabric)
{
    uint32_t local_port_index;
    uint32_t unique_port_index;
    uint32_t max_ports;
    uint32_t port_index;

    max_ports = 0;
    for (local_port_index = 0; local_port_index < fabric->num_local_ports; local_port_index++)
    {
        const ibnd_node_t *node = fabric->fabric_from_local_ports[local_port_index].discovered->nodes;

        while (node != NULL)
        {
            max_ports += node->numports;
            node = node->next;
        }
    }

    fabric->num_unique_ports = 0;
    fabric->unique_ports = calloc (max_ports, sizeof (unique_port_for_counters_t *));
    for (local_port_index = 0; local_port_index < fabric->num_local_ports; local_port_index++)
    {
        fabric_from_local_port_t *const fabric_from_local_port = &fabric->fabric_from_local_ports[local_port_index];
        const ibnd_node_t *node = fabric_from_local_port->discovered->nodes;

        while (node != NULL)
        {
            for (port_index = 0; port_index <= node->numports; port_index++)
            {
                ibnd_port_t *const candidate_port = node->ports[port_index];

                if (candidate_port != NULL)
                {
                    bool found_existing_port = false;

                    for (unique_port_index = 0;
                         (!found_existing_port) && (unique_port_index < fabric->num_unique_ports);
                         unique_port_index++)
                    {
                        ibnd_port_t *const existing_port = fabric->unique_ports[unique_port_index].fabric_port;

                        found_existing_port = (candidate_port->guid == existing_port->guid) &&
                                (candidate_port->portnum == existing_port->portnum);
                    }

                    if (!found_existing_port)
                    {
                        unique_port_for_counters_t *const new_port = &fabric->unique_ports[fabric->num_unique_ports];
                        bool is_switch_base_port_zero = false;


                        if ((candidate_port->node->type == IB_NODE_SWITCH) && (port_index == 0))
                        {
                            uint32_t enhanced_port0;
                            mad_decode_field (candidate_port->node->switchinfo, IB_SW_ENHANCED_PORT0_F, &enhanced_port0);
                            is_switch_base_port_zero = enhanced_port0 == 0;
                        }

                        if (is_switch_base_port_zero)
                        {
                            /* A base switch port 0 doesn't support performance counters, so don't add to the list of unique ports */
                        }
                        else
                        {
                            check_assert (fabric->num_unique_ports < max_ports, "Too many ports");
                            new_port->fabric_port = candidate_port;
                            new_port->fabric_from_local_port = fabric_from_local_port;
                            fabric->num_unique_ports++;
                        }
                    }
                }
            }

            node = node->next;
        }
    }
}

/**
 * @brief Obtain and display the current counter values for one Infiniband port.
 * @details Displays the counters regardless of if the link on the port is active or not.
 *          If the port is connected, displays the lid and port of the remote end.
 * @param[in] port Which Infiniband port to display the counter values for
 */
static void display_port_counters (unique_port_for_counters_t *const port)
{
    ib_portid_t portid;
    uint8_t mad_buf[1024];
    const int timeout = 0;
    uint32_t cap_mask;
    uint32_t port_state;

    /* Determine which counters are supported */
    memset (&portid, 0, sizeof (portid));
    portid.lid = port->fabric_port->base_lid;
    memset (mad_buf, 0, sizeof (mad_buf));
    check_assert (pma_query_via (mad_buf, &portid, port->fabric_port->portnum, timeout,
            CLASS_PORT_INFO, port->fabric_from_local_port->mad_port) != NULL,
            "pma_query_via failed for %s LID %u", port->fabric_from_local_port->local_hca, portid.lid);
    mad_decode_field (mad_buf, IB_CPI_CAPMASK_F, &cap_mask);
    cap_mask = CL_HTON16 (cap_mask);

    /* Determine the port state */
    memset (mad_buf, 0, sizeof (mad_buf));
    check_assert (smp_query_via (mad_buf, &portid, IB_ATTR_PORT_INFO, port->fabric_port->portnum, timeout,
            port->fabric_from_local_port->mad_port) != NULL,
            "smp_query_via failed for %s LID %u", port->fabric_from_local_port->local_hca, portid.lid);
    mad_decode_field (mad_buf, IB_PORT_STATE_F, &port_state);

    printf ("Counters for node \"%s\" port %u LID %u %s", port->fabric_port->node->nodedesc,
            port->fabric_port->portnum, portid.lid, ib_get_port_state_str (port_state));
    if (port->fabric_port->remoteport != NULL)
    {
        printf (" (remote port %u LID %u)", port->fabric_port->remoteport->portnum,
                port->fabric_port->remoteport->base_lid);
    }
    printf ("\n");
    if ((cap_mask & IB_PM_EXT_WIDTH_SUPPORTED) || (cap_mask & IB_PM_EXT_WIDTH_NOIETF_SUP))
    {
        memset (mad_buf, 0, sizeof (mad_buf));
        check_assert (pma_query_via (mad_buf, &portid, port->fabric_port->portnum, timeout,
                IB_GSI_PORT_COUNTERS_EXT, port->fabric_from_local_port->mad_port) != NULL,
                "pma_query_via failed for %s LID %u", port->fabric_from_local_port->local_hca, portid.lid);

        DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_BYTES_F);
        DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_BYTES_F);
        DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_PKTS_F);
        DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_PKTS_F);
        if (cap_mask & IB_PM_EXT_WIDTH_SUPPORTED)
        {
            DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_UPKTS_F);
            DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_UPKTS_F);
            DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_XMT_MPKTS_F);
            DISPLAY_64BIT_FIELD (mad_buf, IB_PC_EXT_RCV_MPKTS_F);
        }
    }

    memset (mad_buf, 0, sizeof (mad_buf));
    check_assert (pma_query_via (mad_buf, &portid, port->fabric_port->portnum, timeout,
            IB_GSI_PORT_COUNTERS, port->fabric_from_local_port->mad_port) != NULL,
            "pma_query_via failed for %s LID %u", port->fabric_from_local_port->local_hca, portid.lid);
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
    if (cap_mask & IB_PM_PC_XMIT_WAIT_SUP)
    {
        DISPLAY_32BIT_FIELD (mad_buf, IB_PC_XMT_WAIT_F);
    }
    printf ("\n");
}


int main (int argc, char *argv[])
{
    discovered_fabric_t fabric;

    discover_fabric (&fabric);
    find_unique_ports (&fabric);
    for (uint32_t unique_port_index = 0; unique_port_index < fabric.num_unique_ports; unique_port_index++)
    {
        display_port_counters (&fabric.unique_ports[unique_port_index]);
    }
    destroy_fabric (&fabric);

    return EXIT_SUCCESS;
}
