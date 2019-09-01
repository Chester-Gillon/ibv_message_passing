/*
 * @file ibv_display_local_infiniband_port_statistics.c
 * @date 28 Jul 2019
 * @author Chester Gillon
 * @details Program which at regular intervals collects and displays the Infiniband port statistics forL
 *          a. All Infiniband ports on the local host.
 *          b. All remote Infiniband ports which are directly connected to the local host.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#include <time.h>
#include <limits.h>
#include <signal.h>

#include <infiniband/verbs.h>
#include <infiniband/mad.h>
#include <infiniband/iba/ib_types.h>
#include <infiniband/ibnetdisc.h>


/* The list of Infiniband port statistics counters which this program collects, with the names based upon the IB_PC_*_F
 * enumerations.
 * STATS_COUNTER_EXT_XMT_DATA / STATS_COUNTER_EXT_RCV_DATA are used instead of
 * STATS_COUNTER_EXT_RCV_BYTES / STATS_COUNTER_EXT_RCV_BYTES, since while the IB_PC_EXT_XMT_BYTES_F / IB_PC_EXT_RCV_BYTES_F
 * enumerations contain the term "bytes" the counters actually measure the number of 32-bit words. */
typedef enum
{
    STATS_COUNTER_EXT_XMT_DATA,
    STATS_COUNTER_EXT_RCV_DATA,
    STATS_COUNTER_EXT_XMT_PKTS,
    STATS_COUNTER_EXT_RCV_PKTS,
    STATS_COUNTER_EXT_XMT_UPKTS,
    STATS_COUNTER_EXT_RCV_UPKTS,
    STATS_COUNTER_EXT_XMT_MPKTS,
    STATS_COUNTER_EXT_RCV_MPKTS,
    STATS_COUNTER_ERR_SYM,
    STATS_COUNTER_LINK_RECOVERS,
    STATS_COUNTER_LINK_DOWNED,
    STATS_COUNTER_ERR_RCV,
    STATS_COUNTER_ERR_PHYSRCV,
    STATS_COUNTER_ERR_SWITCH_REL,
    STATS_COUNTER_XMT_DISCARDS,
    STATS_COUNTER_ERR_XMTCONSTR,
    STATS_COUNTER_ERR_RCVCONSTR,
    STATS_COUNTER_ERR_LOCALINTEG,
    STATS_COUNTER_ERR_EXCESS_OVR,
    STATS_COUNTER_VL15_DROPPED,
    STATS_COUNTER_XMT_WAIT,

    /* Used to size arrays of counters */
    STATS_COUNTER_ARRAY_SIZE
} ibv_port_statistics_counters_t;


/** The period in seconds between collecting Infiniband port statistics */
#define STATISTICS_COLLECTION_PERIOD_SECS 10


/* The statistics sample for one type of counter for one Infiniband port */
typedef struct
{
    /* If true the sample is valid */
    bool valid;
    /* The current counter value */
    uint64_t current_counter_value;
} port_counter_sample_t;


/* The collection of statistics samples for one Infiniband port */
typedef struct
{
    /* The port has been identified as being active, meaning the port_num and lid are valid.
     * No dynamic reconfiguration of the Infiniband fabric is expected, and so port_num and lid are only set once. */
    bool port_identified;
    /* The Infiniband port number of the node */
    uint8_t port_num;
    /* The Infiniband LID of the port */
    uint16_t lid;
    /* Array of sampled statistics counters for the port, which are updated on each statistics collection interval */
    port_counter_sample_t counters[STATS_COUNTER_ARRAY_SIZE];
} port_statistics_sample_t;


/* The statistics for one sample interval associated with one local Infiniband port */
typedef struct
{
    /* The local NUMA node for the Infiniband device. */
    int local_numa_node;
    /* The statistics for the port on an Infiniband device on the local host */
    port_statistics_sample_t local;
    /* The statistics for the remote port connected to the local port.
     * While the throughput related counters are expected to be a mirror-image for tx/rx on the local/remote ports, the
     * error counts may only be recorded on the local or remote ports. */
    port_statistics_sample_t remote;
} statistics_sample_t;


/* The context used to collect statistics for one local Infiniband port */
typedef struct
{
    /** The local Infiniband device context, to obtain the port attributes */
    struct ibv_context *device_context;
    /** The Infiniband device name of the local HCA */
    char *local_hca;
    /** Used to send MAD requests to query the performance counters of the local and connected remote port */
    struct ibmad_port *mad_port;
    /* The statistics for the local Infiniband port */
    statistics_sample_t statistics;
} local_port_statistics_context_t;


/** Maximum number of Infiniband ports on the local host */
#define MAX_LOCAL_PORTS 8u


/** The context used to collect statistics for all Infiniband ports on Infiniband devices on the local host */
typedef struct
{
    /** The number of Infiniband devices on the local host */
    int num_ibv_devices;
    /** The list of Infiniband devices on the local host */
    struct ibv_device **ibv_device_list;
    /** The open Infiniband device contents on the local host, for querying attributes */
    struct ibv_context *device_contexts[MAX_LOCAL_PORTS];
    /** The total number of ports on the local host */
    uint32_t num_local_ports;
    /* The statistics for all local Infiniband ports */
    local_port_statistics_context_t local_ports[MAX_LOCAL_PORTS];
} local_host_port_statistics_context_t;


/** Defines how one ibv_port_statistics_counters_t is to be extracted from a MAD value */
typedef struct
{
    /** The MAD field enumeration for the counter */
    enum MAD_FIELDS mad_field;
    /** When true the counter is an extended width 64-bit counter, when false is a 32-bit counter */
    bool extended_width;
    /** The capability mask which defines when an Infiniband port supports this counter.
     *  When non-zero the counter is supported when (cap_mask & supported_cap_mask) != 0
     *  When zero means the counter is always supported.  */
    uint16_t supported_cap_mask;
} port_statistics_counter_definition_t;


/** Lookup table which for each ibv_port_statistics_counters_t enumeration defines how the field is to be extracted
 *  from a MAD value in a performance monitoring query. */
static const port_statistics_counter_definition_t statistics_counter_definitions[STATS_COUNTER_ARRAY_SIZE] =
{
    /* 64-bit counters for "bytes" and total packets transmit/received by the port.
     * While the underlying MAD IB_PC_EXT_XMT_BYTES_F and IB_PC_EXT_RCV_BYTES_F are named "bytes" these counters
     * are actually a count of 32-bit words. */
    [STATS_COUNTER_EXT_XMT_DATA] =
    {
        .mad_field = IB_PC_EXT_XMT_BYTES_F,
        .extended_width = true,
        .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED | IB_PM_EXT_WIDTH_NOIETF_SUP
    },
    [STATS_COUNTER_EXT_RCV_DATA] =
    {
        .mad_field = IB_PC_EXT_RCV_BYTES_F,
        .extended_width = true,
        .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED | IB_PM_EXT_WIDTH_NOIETF_SUP
    },
    [STATS_COUNTER_EXT_XMT_PKTS] =
    {
         .mad_field = IB_PC_EXT_XMT_PKTS_F,
         .extended_width = true,
         .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED | IB_PM_EXT_WIDTH_NOIETF_SUP
    },
    [STATS_COUNTER_EXT_RCV_PKTS] =
    {
        .mad_field = IB_PC_EXT_RCV_PKTS_F,
        .extended_width = true,
        .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED | IB_PM_EXT_WIDTH_NOIETF_SUP
    },

    /* 64-bit counters for the total unicast / multicast packets transmitted / received by the port */
    [STATS_COUNTER_EXT_XMT_UPKTS] =
    {
        .mad_field = IB_PC_EXT_XMT_UPKTS_F,
        .extended_width = true,
        .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED
    },
    [STATS_COUNTER_EXT_RCV_UPKTS] =
    {
        .mad_field = IB_PC_EXT_RCV_UPKTS_F,
        .extended_width = true,
        .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED
    },
    [STATS_COUNTER_EXT_XMT_MPKTS] =
    {
        .mad_field = IB_PC_EXT_XMT_MPKTS_F,
        .extended_width = true,
        .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED
    },
    [STATS_COUNTER_EXT_RCV_MPKTS] =
    {
        .mad_field = IB_PC_EXT_RCV_MPKTS_F,
        .extended_width = true,
        .supported_cap_mask = IB_PM_EXT_WIDTH_SUPPORTED
    },

    /* 32-bit error counters which are defined for all ports */
    [STATS_COUNTER_ERR_SYM] =
    {
        .mad_field = IB_PC_ERR_SYM_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_LINK_RECOVERS] =
    {
        .mad_field = IB_PC_LINK_RECOVERS_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_LINK_DOWNED] =
    {
        .mad_field = IB_PC_LINK_DOWNED_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_ERR_RCV] =
    {
        .mad_field = IB_PC_ERR_RCV_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_ERR_PHYSRCV] =
    {
        .mad_field = IB_PC_ERR_PHYSRCV_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_ERR_SWITCH_REL] =
    {
        .mad_field = IB_PC_ERR_SWITCH_REL_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_XMT_DISCARDS] =
    {
        .mad_field = IB_PC_XMT_DISCARDS_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_ERR_XMTCONSTR] =
    {
        .mad_field = IB_PC_ERR_XMTCONSTR_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_ERR_RCVCONSTR] =
    {
        .mad_field = IB_PC_ERR_RCVCONSTR_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_ERR_LOCALINTEG] =
    {
        .mad_field = IB_PC_ERR_LOCALINTEG_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_ERR_EXCESS_OVR] =
    {
        .mad_field = IB_PC_ERR_EXCESS_OVR_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },
    [STATS_COUNTER_VL15_DROPPED] =
    {
        .mad_field = IB_PC_VL15_DROPPED_F,
        .extended_width = false,
        .supported_cap_mask = 0
    },

    /* 32-bit count for the number of times transmit had to wait */
    [STATS_COUNTER_XMT_WAIT] =
    {
        .mad_field = IB_PC_XMT_WAIT_F,
        .extended_width = false,
        .supported_cap_mask = IB_PM_PC_XMIT_WAIT_SUP
    }
};


/** Set from a signal handler to request that the statistics collection is stopped */
static volatile bool exit_requested;


/**
 * @brief Signal handler to request collection of statistics is stopped
 */
static void stop_statistics_collection_handler (const int sig)
{
    exit_requested = true;
}


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
 * @brief Initialise the context to collect statistics for all Infiniband ports on Infiniband devices on the local host.
 * @details Finds the names for the local Infiniband devices and their local port numbers, and opens the MAD ports used
 *          to query the port counters.
 * @param context[out] The context to initialise
 */
static void find_local_ports (local_host_port_statistics_context_t *const context)
{
    int device_index;
    int rc;
    char sysfs_path[PATH_MAX];
    FILE *sysfs_file;
    uint8_t port_num;
    int device_numa_node;
    int mgmt_classes[3] = {IB_SMI_CLASS, IB_SMI_DIRECT_CLASS, IB_PERFORMANCE_CLASS};

    memset (context, 0, sizeof (*context));
    context->num_local_ports = 0;
    context->ibv_device_list = ibv_get_device_list (&context->num_ibv_devices);
    if (context->ibv_device_list != NULL)
    {
        check_assert (context->num_ibv_devices < MAX_LOCAL_PORTS, "Too many local devices");
        for (device_index = 0; device_index < context->num_ibv_devices; device_index++)
        {
            struct ibv_device *const device = context->ibv_device_list[device_index];
            context->device_contexts[device_index] = ibv_open_device (device);
            struct ibv_device_attr device_attributes;

            check_assert (context != NULL, "ibv_open_device failed");
            rc = ibv_query_device (context->device_contexts[device_index], &device_attributes);
            check_assert (rc == 0, "ibv_query_device failed");

            /* Find the local NUMA for the Infiniband device */
            snprintf (sysfs_path, sizeof (sysfs_path), "%s/device/numa_node", device->ibdev_path);
            sysfs_file = fopen (sysfs_path, "r");
            const int num_items = fscanf (sysfs_file, "%d", &device_numa_node);
            check_assert (num_items == 1, "failed to parse %s", sysfs_path);
            fclose (sysfs_file);

            for (port_num = 1; port_num <= device_attributes.phys_port_cnt; port_num++)
            {
                local_port_statistics_context_t *const local_port = &context->local_ports[context->num_local_ports];

                check_assert (context->num_local_ports < MAX_LOCAL_PORTS, "Too many local ports");
                local_port->device_context = context->device_contexts[device_index];
                local_port->local_hca = strdup (ibv_get_device_name (device));
                local_port->statistics.local_numa_node = device_numa_node;
                local_port->statistics.local.port_num = port_num;
                local_port->mad_port =
                        mad_rpc_open_port (local_port->local_hca, local_port->statistics.local.port_num, mgmt_classes,
                                sizeof (mgmt_classes) / sizeof (mgmt_classes[0]));
                check_assert (local_port->mad_port != NULL, "mad_rpc_open_port failed for %s port %u",
                        local_port->local_hca, local_port->statistics.local.port_num);

                context->num_local_ports++;
            }
        }
    }
}


/**
 * @brief Identify the LID of all local Infiniband ports, and the directly connected remote ports.
 * @details This is to get the required information to collect Infiniband port counters.
 *          No dynamic reconfiguration of the Infiniband fabric is expected, and so this function takes no further action
 *          once all ports have been identified.
 * @param[in/out] context The context to identify the ports for.
 */
static void identify_connected_ports (local_host_port_statistics_context_t *const context)
{
    struct ibv_port_attr port_attributes;
    uint32_t local_port_index;
    int rc;

    for (local_port_index = 0; local_port_index < context->num_local_ports; local_port_index++)
    {
        local_port_statistics_context_t *const local_port = &context->local_ports[local_port_index];

        if (!local_port->statistics.local.port_identified)
        {
            /* First get the LID for the local port, which is valid when the port is active */
            rc = ibv_query_port (local_port->device_context, local_port->statistics.local.port_num, &port_attributes);
            check_assert (rc == 0, "ibv_query_port failed");
            if (port_attributes.state == IBV_PORT_ACTIVE)
            {
                local_port->statistics.local.lid = port_attributes.lid;
                local_port->statistics.local.port_identified = true;
            }
        }

        if (local_port->statistics.local.port_identified && !local_port->statistics.remote.port_identified)
        {
            /* Discover the fabric from the local port, and find the remote port which is directly connected to the local
             * port by matching the lid and port number.
             *
             * The guid is not matched, since the guid is port specific and the verbs API doesn't appear to return the
             * port guid. */
            ibnd_config_t config = {0};

            ibnd_fabric_t *const discovered =
                    ibnd_discover_fabric (local_port->local_hca, local_port->statistics.local.port_num, NULL, &config);

            if (discovered != NULL)
            {
                const ibnd_node_t *node = discovered->nodes;

                while ((node != NULL) && (!local_port->statistics.remote.port_identified))
                {
                    for (int port_index = 0;
                         (!local_port->statistics.remote.port_identified) && (port_index <= node->numports);
                         port_index++)
                    {
                        ibnd_port_t *const candidate_port = node->ports[port_index];

                        if (candidate_port != NULL)
                        {
                            if (candidate_port->remoteport != NULL)
                            {
                                if ((candidate_port->remoteport->base_lid == local_port->statistics.local.lid) &&
                                    (candidate_port->remoteport->portnum == local_port->statistics.local.port_num))
                                {
                                    /* Store the identity of the remote port */
                                    local_port->statistics.remote.lid = candidate_port->base_lid;
                                    local_port->statistics.remote.port_num = candidate_port->portnum;
                                    local_port->statistics.remote.port_identified = true;
                                }
                            }
                        }
                    }

                    node = node->next;
                }
                ibnd_destroy_fabric (discovered);
            }
        }

    }
}


/**
 * @brief Store the value of one Inifiniband port statistics counter, if valid
 * @param[in] pma_query_rc If non-NULL the pma_query_via() call succeeded and mad_buf is valid.
 * @param[in] cap_mask The capability mask for the port, which indicates which counters are valid.
 * @param[in] mad_buf The buffer for the performance counter query, from which to decode the statistics counter value.
 * @param[in] counter_id Which statistics counter to store the value for.
 * @param[in] statistics Where to store the statistics counter value.
 */
static void store_statistics_counter (uint8_t *const pma_query_rc, const uint32_t cap_mask, uint8_t *const mad_buf,
                                      const ibv_port_statistics_counters_t counter_id,
                                      port_statistics_sample_t *const statistics)
{
    const port_statistics_counter_definition_t *const counter_def = &statistics_counter_definitions[counter_id];
    port_counter_sample_t *const sample = &statistics->counters[counter_id];
    uint32_t field_value32 = 0;
    uint64_t field_value64 = 0;

    sample->valid = false;
    if (pma_query_rc != NULL)
    {
        if ((counter_def->supported_cap_mask == 0) || ((cap_mask & counter_def->supported_cap_mask) != 0))
        {
            if (counter_def->extended_width)
            {
                mad_decode_field (mad_buf, counter_def->mad_field, &field_value64);
                sample->current_counter_value = field_value64;
            }
            else
            {
                mad_decode_field (mad_buf, counter_def->mad_field, &field_value32);
                sample->current_counter_value = field_value32;
            }
            sample->valid = true;
        }
    }
}


/**
 * @brief Obtain the current Infiniband statistics counter values for one Infiniband port.
 * @details To allow this program to run in the event that Infiniband errors are being injected, e.g. links reset, then
 *          a failure of an Inifiniband management query isn't considered a fatal error, but instead an error trying to
 *          retrieve the current counter values just results in the statistics counter values being marked as invalid.
 * @param mad_port[IN] Used as the source port for Infiniband management query.
 * @param statistics[IN/OUT] Where to store the Infiniband statistics counter values.
 *                           On entry identifies the port to query.
 *                           On exit updated with the current counter values, with an indication if valid.
 */
static void get_port_statistics (const struct ibmad_port *mad_port, port_statistics_sample_t *const statistics)
{
    uint8_t mad_buf[1024];
    const int timeout = 0;
    uint32_t cap_mask;
    uint8_t *pma_query_rc = NULL;
    ibv_port_statistics_counters_t counter_id;

    /* Initialise to indicate no valid statistics */
    memset (statistics->counters, 0, sizeof (statistics->counters));

    /* Only attempt to get the port statistics once have identified how to address the port */
    if (statistics->port_identified)
    {
        ib_portid_t portid =
        {
            .lid = statistics->lid
        };

        /* Determine which counters are supported */
        memset (mad_buf, 0, sizeof (mad_buf));
        pma_query_rc = pma_query_via (mad_buf, &portid, statistics->port_num, timeout, CLASS_PORT_INFO, mad_port);
        if (pma_query_rc != NULL)
        {
            mad_decode_field (mad_buf, IB_CPI_CAPMASK_F, &cap_mask);
            cap_mask = CL_HTON16 (cap_mask);

            /* Obtain the extended (64-bit) port counters */
            memset (mad_buf, 0, sizeof (mad_buf));
            pma_query_rc = pma_query_via (mad_buf, &portid, statistics->port_num, timeout, IB_GSI_PORT_COUNTERS_EXT, mad_port);
            for (counter_id = 0; counter_id < STATS_COUNTER_ARRAY_SIZE; counter_id++)
            {
                if (statistics_counter_definitions[counter_id].extended_width)
                {
                    store_statistics_counter (pma_query_rc, cap_mask, mad_buf, counter_id, statistics);
                }
            }

            /* Obtain the 32-bit port counters */
            memset (mad_buf, 0, sizeof (mad_buf));
            pma_query_rc = pma_query_via (mad_buf, &portid, statistics->port_num, timeout, IB_GSI_PORT_COUNTERS, mad_port);
            for (counter_id = 0; counter_id < STATS_COUNTER_ARRAY_SIZE; counter_id++)
            {
                if (!statistics_counter_definitions[counter_id].extended_width)
                {
                    store_statistics_counter (pma_query_rc, cap_mask, mad_buf, counter_id, statistics);
                }
            }
        }
    }
}


/**
 * @brief Display in tabular form the current values for collected Infiniband port counters.
 * @details Counters with no current valid valid are displayed as blank
 * @param[in] num_collection_iterations How many collection intervals have occurred, used to display the number of
 *                                      seconds since statistics collection was started.
 * @param[in] context Contains all Infiniband port counter statistics to display.
 */
static void display_collected_statistics (const uint32_t num_collection_iterations,
                                          const local_host_port_statistics_context_t *const context)
{
    uint32_t local_port_index;
    ibv_port_statistics_counters_t counter_id;
    const int counter_name_width = 29;
    const int local_numa_node_width = 9;
    const int local_port_width = 5;
    const int local_lid_width = 5;
    const int local_counter_value_width = 20;
    const int remote_port_width = 6;
    const int remote_lid_width = 6;
    const int remote_counter_value_width = local_counter_value_width;

    printf ("\nStatistics after %" PRIu32 " seconds\n", num_collection_iterations * STATISTICS_COLLECTION_PERIOD_SECS);
    printf ("%*s  %*s  %*s  %*s  %*s  %*s  %*s  %*s\n",
            -counter_name_width, "",
            -local_numa_node_width, "Local",
            -local_port_width, "Local",
            -local_lid_width, "Local",
            -local_counter_value_width, "Local",
            -remote_port_width, "Remote",
            -remote_lid_width, "Remote",
            -remote_counter_value_width, "Remote");
    printf ("%*s  %*s  %*s  %*s  %*s  %*s  %*s  %*s\n",
            -counter_name_width, "Counter name",
            -local_numa_node_width, "NUMA node",
            -local_port_width, "Port",
            -local_lid_width, "LID",
            -local_counter_value_width, "Counter value",
            -remote_port_width, "Port",
            -remote_lid_width, "LID",
            -remote_counter_value_width, "Counter value");

    for (local_port_index = 0; local_port_index < context->num_local_ports; local_port_index++)
    {
        const statistics_sample_t *const statistics = &context->local_ports[local_port_index].statistics;

        for (counter_id = 0; counter_id < STATS_COUNTER_ARRAY_SIZE; counter_id++)
        {
            const port_counter_sample_t *const local = &statistics->local.counters[counter_id];
            const port_counter_sample_t *const remote = &statistics->remote.counters[counter_id];

            /* Fields which are always valid */
            printf ("%*s  %*d  %*" PRIu16,
                    -counter_name_width, mad_field_name (statistics_counter_definitions[counter_id].mad_field),
                    local_numa_node_width, statistics->local_numa_node,
                    local_port_width, statistics->local.port_num);

            /* Local LID is only valid once the port has been identified */
            if (statistics->local.port_identified)
            {
                printf ("  %*u", local_lid_width, statistics->local.lid);
            }
            else
            {
                printf ("  %*s", -local_lid_width, "");
            }

            /* Local port counter value has its own validity flag */
            if (local->valid)
            {
                printf ("  %*" PRIu64, local_counter_value_width, local->current_counter_value);
            }
            else
            {
                printf ("  %*s", -local_counter_value_width, "");
            }

            /* Remote port number and LID are only value once the port has been identified */
            if (statistics->remote.port_identified)
            {
                printf ("  %*" PRIu16 "  %*u",
                        remote_port_width, statistics->remote.port_num,
                        remote_lid_width, statistics->remote.lid);
            }
            else
            {
                printf ("  %*s  %*s", -remote_port_width, "", -remote_lid_width, "");
            }

            /* Local port counter value has its own validity flag */
            if (remote->valid)
            {
                printf ("  %*" PRIu64, remote_counter_value_width, remote->current_counter_value);
            }
            else
            {
                printf ("  %*s", -remote_counter_value_width, "");
            }
            printf ("\n");
        }
    }
}


int main (int argc, char *argv[])
{
    local_host_port_statistics_context_t context;
    uint32_t local_port_index;
    int rc;
    struct timespec next_sample_time;
    uint32_t num_collection_iterations;
    struct sigaction action;

    find_local_ports (&context);
    if (context.num_local_ports > 0)
    {
        /* Install a signal handler to allow a request to stop of statistics collection */
        printf ("Press Ctrl-C to stop the Infiniband port statistics collection\n");
        memset (&action, 0, sizeof (action));
        action.sa_handler = stop_statistics_collection_handler;
        action.sa_flags = SA_RESTART;
        rc = sigaction (SIGINT, &action, NULL);
        check_assert (rc == 0, "sigaction");

        rc = clock_gettime (CLOCK_MONOTONIC, &next_sample_time);
        check_assert (rc == 0, "clock_gettime");

        /* Report Infiniband port statistics at regular intervals until requested to stop */
        num_collection_iterations = 0;
        do
        {
            /* Get the current statistics for all local ports */
            identify_connected_ports (&context);
            for (local_port_index = 0; local_port_index < context.num_local_ports; local_port_index++)
            {
                local_port_statistics_context_t *const local_port = &context.local_ports[local_port_index];

                get_port_statistics (local_port->mad_port, &local_port->statistics.local);
                get_port_statistics (local_port->mad_port, &local_port->statistics.remote);
            }

            display_collected_statistics (num_collection_iterations, &context);

            /* Wait until the next collection interval */
            next_sample_time.tv_sec += STATISTICS_COLLECTION_PERIOD_SECS;
            clock_nanosleep (CLOCK_MONOTONIC, TIMER_ABSTIME, &next_sample_time, NULL);
            num_collection_iterations++;
        } while (!exit_requested);

        printf ("\n");
    }
    else
    {
        printf ("No local Infiniband ports found\n");
    }

    return EXIT_SUCCESS;
}
