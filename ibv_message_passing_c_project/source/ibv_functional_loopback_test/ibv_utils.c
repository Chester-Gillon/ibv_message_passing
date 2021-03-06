/*
 * @file ibv_utils.c
 * @date: 9 Sep 2017
 * @author Chester Gillon
 * @brief Utility functions for Infiniband tests
 */

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <unistd.h>
#include <sys/resource.h>

#include <infiniband/verbs.h>
#include <infiniband/mad.h>
#include <infiniband/iba/ib_types.h>

#include "ibv_utils.h"


/** Defines one entry to map an enumeration value to its name */
#define ENUM_LIST_INIT(enumeration) {enumeration, #enumeration}
#define ENUM_LIST_END {0, NULL}
typedef struct
{
    /** The enumeration value */
    int enum_value;
    /** The enumeration name, which is NULL for the end of the list */
    const char *enum_name;
} enum_list_entry;

/** Enumerations for ibv_atomic_cap */
static const enum_list_entry ibv_atomic_cap_enums[] =
{
    ENUM_LIST_INIT (IBV_ATOMIC_NONE),
    ENUM_LIST_INIT (IBV_ATOMIC_HCA),
    ENUM_LIST_INIT (IBV_ATOMIC_GLOB),
    ENUM_LIST_END
};

/** Enumerations for ibv_device_cap_flags
 *  @todo This list of enumerations was taken from the verbs.h installed from libibverbs-dev_17.1-1
 *        With a Mellanox Technologies MT26428 the following bits are set in device_cap_flags for which there is no enumeration:
 *        - BIT15 - IB_DEVICE_LOCAL_DMA_LKEY in some versions of ib_verbs.h
 *        - BIT18 - IBV_DEVICE_IP_CSUM in some versions of ib_verbs.h
 *        - BIT19 - IB_DEVICE_UD_IP_CSUM in some versions of ib_verbs.h
 *        - BIT21 - IBV_DEVICE_MEM_MGT_EXTENSIONS in some other versions of verbs.h
 *        - BIT22 - IB_DEVICE_BLOCK_MULTICAST_LOOPBACK in some versions of ib_verbs.h
 *        - BIT26 - IBV_DEVICE_RAW_IP_CSUM in some other versions of verbs.h
 *          */
static const enum_list_entry ibv_device_cap_flags_enums[] =
{
    ENUM_LIST_INIT (IBV_DEVICE_RESIZE_MAX_WR),
    ENUM_LIST_INIT (IBV_DEVICE_BAD_PKEY_CNTR),
    ENUM_LIST_INIT (IBV_DEVICE_BAD_QKEY_CNTR),
    ENUM_LIST_INIT (IBV_DEVICE_RAW_MULTI),
    ENUM_LIST_INIT (IBV_DEVICE_AUTO_PATH_MIG),
    ENUM_LIST_INIT (IBV_DEVICE_CHANGE_PHY_PORT),
    ENUM_LIST_INIT (IBV_DEVICE_UD_AV_PORT_ENFORCE),
    ENUM_LIST_INIT (IBV_DEVICE_CURR_QP_STATE_MOD),
    ENUM_LIST_INIT (IBV_DEVICE_SHUTDOWN_PORT),
    ENUM_LIST_INIT (IBV_DEVICE_INIT_TYPE),
    ENUM_LIST_INIT (IBV_DEVICE_PORT_ACTIVE_EVENT),
    ENUM_LIST_INIT (IBV_DEVICE_SYS_IMAGE_GUID),
    ENUM_LIST_INIT (IBV_DEVICE_RC_RNR_NAK_GEN),
    ENUM_LIST_INIT (IBV_DEVICE_SRQ_RESIZE),
    ENUM_LIST_INIT (IBV_DEVICE_N_NOTIFY_CQ),
    ENUM_LIST_INIT (IBV_DEVICE_XRC),
    ENUM_LIST_INIT (IBV_DEVICE_MANAGED_FLOW_STEERING),
    ENUM_LIST_END
};

/** Enumerations for ibv_port_state */
static const enum_list_entry ibv_port_state_enums[] =
{
    ENUM_LIST_INIT (IBV_PORT_NOP),
    ENUM_LIST_INIT (IBV_PORT_DOWN),
    ENUM_LIST_INIT (IBV_PORT_INIT),
    ENUM_LIST_INIT (IBV_PORT_ARMED),
    ENUM_LIST_INIT (IBV_PORT_ACTIVE),
    ENUM_LIST_INIT (IBV_PORT_ACTIVE_DEFER),
    ENUM_LIST_END
};

/** Enumerations for ibv_mtu */
static const enum_list_entry ibv_mtu_enums[] =
{
    ENUM_LIST_INIT (IBV_MTU_256),
    ENUM_LIST_INIT (IBV_MTU_512),
    ENUM_LIST_INIT (IBV_MTU_1024),
    ENUM_LIST_INIT (IBV_MTU_2048),
    ENUM_LIST_INIT (IBV_MTU_4096),
    ENUM_LIST_END
};

/** Enumerations for ibv_qp_state */
static const enum_list_entry ibv_qp_state_enums[] =
{
    ENUM_LIST_INIT (IBV_QPS_RESET),
    ENUM_LIST_INIT (IBV_QPS_INIT),
    ENUM_LIST_INIT (IBV_QPS_RTR),
    ENUM_LIST_INIT (IBV_QPS_RTS),
    ENUM_LIST_INIT (IBV_QPS_SQD),
    ENUM_LIST_INIT (IBV_QPS_SQE),
    ENUM_LIST_INIT (IBV_QPS_ERR),
    ENUM_LIST_INIT (IBV_QPS_UNKNOWN),
    ENUM_LIST_END
};

/** Enumerations for link layer */
static const enum_list_entry ibv_link_layer_enums[] =
{
    ENUM_LIST_INIT (IBV_LINK_LAYER_UNSPECIFIED),
    ENUM_LIST_INIT (IBV_LINK_LAYER_INFINIBAND),
    ENUM_LIST_INIT (IBV_LINK_LAYER_ETHERNET),
    ENUM_LIST_END
};


/**
 * @brief Perform a page size aligned allocation
 * @param[in] size The size of the allocated
 * @return Returns a pointer to page aligned allocation
 */
void *page_aligned_alloc (const size_t size)
{
    int rc;
    const size_t page_size = sysconf (_SC_PAGE_SIZE);
    void *buffer;

    rc = posix_memalign (&buffer, page_size, size);
    if (rc != 0)
    {
        errno = rc;
        perror ("posix_memalign failed");
        exit (EXIT_FAILURE);
    }

    return buffer;
}

/**
 * @brief Perform a page size aligned allocation, and zero the allocated memory
 * @param[in] nmemb Number of elements in the array to allocate
 * @param[in] size The size of each element in the array to allocate
 * @return Return a pointer to the zeroed page aligned allocation
 */
void *page_aligned_calloc (const size_t nmemb, const size_t size)
{
    void *const buffer = page_aligned_alloc (nmemb * size);

    memset (buffer, 0, size);

    return buffer;
}

/**
 * @brief Perform a cache line size aligned allocation
 * @param[in] size The size of the allocated
 * @return Returns a pointer to page aligned allocation
 */
void *cache_line_aligned_alloc (const size_t size)
{
    int rc;
    void *buffer;

    rc = posix_memalign (&buffer, CACHE_LINE_SIZE_BYTES, size);
    if (rc != 0)
    {
        errno = rc;
        perror ("posix_memalign failed");
        exit (EXIT_FAILURE);
    }

    return buffer;
}

/**
 * @brief Perform a cache line size aligned allocation, and zero the allocated memory
 * @param[in] nmemb Number of elements in the array to allocate
 * @param[in] size The size of each element in the array to allocate
 * @return Return a pointer to the zeroed page aligned allocation
 */
void *cache_line_aligned_calloc (const size_t nmemb, const size_t size)
{
    void *const buffer = cache_line_aligned_alloc (nmemb * size);

    memset (buffer, 0, size);

    return buffer;
}

/**
 * @brief Display the name of an enumeration value
 * @param[in] enum_value The enumeration value to display the name for
 * @param[in] enum_list The list of enumeration values to names
 */
static void display_enumeration_name (const int enum_value, const enum_list_entry *enum_list)
{
    bool enum_found = false;
    const enum_list_entry *enum_entry = enum_list;

    while (!enum_found && (enum_entry->enum_name != NULL))
    {
        if (enum_entry->enum_value == enum_value)
        {
            enum_found = true;
        }
        else
        {
            enum_entry++;
        }
    }

    if (enum_found)
    {
        printf ("%s", enum_entry->enum_name);
    }
    else
    {
        printf ("Unknown enum %d", enum_value);
    }
}

/**
 * @brief Display the contents of flags which are encoded by enumeration bit masks
 * @param[in] enum_flags The enumerated flags to display
 * @param[in] enum_list The list of enumeration bit masks
 */
static void display_enumeration_flags (const int enum_flags, const enum_list_entry *enum_list)
{
    int num_bits_set;
    int bit_num;

    num_bits_set = 0;
    for (bit_num = 0; bit_num < 32; bit_num++)
    {
        const int bit_mask = 1 << bit_num;

        if ((enum_flags & bit_mask) != 0)
        {
            bool enum_found = false;
            const enum_list_entry *enum_entry = enum_list;

            num_bits_set++;
            while (!enum_found && (enum_entry->enum_name != NULL))
            {
                if (enum_entry->enum_value == bit_mask)
                {
                    enum_found = true;
                }
                else
                {
                    enum_entry++;
                }
            }

            if (num_bits_set > 1)
            {
                printf ("|");
            }
            if (enum_found)
            {
                printf ("%s", enum_entry->enum_name);
            }
            else
            {
                printf ("BIT%d", bit_num);
            }
        }
    }
}

/**
 * @brief Display the attributes of an Infiniband device, for diagnostic information
 * @param[in] device The device for which the attributes are displayed
 * @param[in] device_attr The device attributes to display
 */
void display_ibv_device_attributes (const struct ibv_context *const device, const struct ibv_device_attr *const device_attr)
{
    printf ("Attributes of device %s:\n", ibv_get_device_name (device->device));
    printf (" num_comp_vectors=%d fw_ver=%s\n", device->num_comp_vectors, device_attr->fw_ver);
    printf (" node_guid=0x%llx sys_image_guid=0x%llx\n", device_attr->node_guid, device_attr->sys_image_guid);
    printf (" max_mr_size=%lu page_size_cap=%lu vendor_id=0x%x vendor_part_id=0x%x hw_ver=%u\n",
            device_attr->max_mr_size, device_attr->page_size_cap, device_attr->vendor_id, device_attr->vendor_part_id, device_attr->hw_ver);
    printf (" max_qp=%d max_qp_wr=%d max_sge=%d max_sge_rd=%d\n",
            device_attr->max_qp, device_attr->max_qp_wr, device_attr->max_sge, device_attr->max_sge_rd);
    printf ("device_cap_flags=");
    display_enumeration_flags (device_attr->device_cap_flags, ibv_device_cap_flags_enums);
    printf ("\n");
    printf (" max_cq=%d max_cqe=%d max_mr=%d max_pd=%d\n",
            device_attr->max_cq, device_attr->max_cqe, device_attr->max_mr, device_attr->max_pd);
    printf (" max_qp_rd_atom=%d max_ee_rd_atom=%d max_res_rd_atom=%d max_qp_init_rd_atom=%d\n",
            device_attr->max_qp_rd_atom, device_attr->max_ee_rd_atom, device_attr->max_res_rd_atom, device_attr->max_qp_init_rd_atom);
    printf (" max_ee_init_rd_atom=%d atomic_cap=", device_attr->max_ee_init_rd_atom);
    display_enumeration_name (device_attr->atomic_cap, ibv_atomic_cap_enums);
    printf ("\n");
    printf (" max_ee=%d max_rdd=%d max_mw=%d max_raw_ipv6_qp=%d max_raw_ethy_qp=%d\n",
            device_attr->max_ee, device_attr->max_rdd, device_attr->max_mw, device_attr->max_raw_ipv6_qp, device_attr->max_raw_ethy_qp);
    printf (" max_mcast_grp=%d max_mcast_qp_attach=%d max_total_mcast_qp_attach=%d\n",
            device_attr->max_mcast_grp, device_attr->max_mcast_qp_attach, device_attr->max_total_mcast_qp_attach);
    printf (" max_ah=%d max_fmr=%d max_map_per_fmr=%d max_srq=%d max_srq_wr=%d\n",
            device_attr->max_ah, device_attr->max_fmr, device_attr->max_map_per_fmr, device_attr->max_srq, device_attr->max_srq_wr);
    printf (" max_srq_sge=%d max_pkeys=%u local_ca_ack_delay=%u phys_port_cnt=%u\n",
            device_attr->max_srq_sge, device_attr->max_pkeys, device_attr->local_ca_ack_delay, device_attr->phys_port_cnt);

}

/**
 * @brief Display the attributes of one port on an Infiniband device, for diagnostic information
 * @param[in] port_attr The port attributes to display
 */
void display_ibv_port_attributes (const struct ibv_port_attr *const port_attr)
{
    printf (" state=");
    display_enumeration_name (port_attr->state, ibv_port_state_enums);
    printf (" max_mtu=");
    display_enumeration_name (port_attr->max_mtu, ibv_mtu_enums);
    printf (" active_mtu=");
    display_enumeration_name (port_attr->active_mtu, ibv_mtu_enums);
    printf ("\n");
    printf (" gid_tbl_len=%d port_cap_flags=0x%x max_msg_sz=%u\n",
            port_attr->gid_tbl_len, port_attr->port_cap_flags, port_attr->max_msg_sz);
    printf (" bad_pkey_cntr=%u qkey_viol_cntr=%u pkey_tbl_len=%u lid=%u sm_lid=%u\n",
            port_attr->bad_pkey_cntr, port_attr->qkey_viol_cntr, port_attr->pkey_tbl_len, port_attr->lid, port_attr->sm_lid);
    printf (" lmc=%u max_vl_num=%u sm_sl=%u subnet_timeout=%u init_type_reply=%u\n",
            port_attr->lmc, port_attr->max_vl_num, port_attr->sm_sl, port_attr->subnet_timeout, port_attr->init_type_reply);
    printf (" active_width=%u active_speed=%u phys_state=%u link_layer=",
            port_attr->active_width, port_attr->active_speed, port_attr->phys_state);
    display_enumeration_name (port_attr->link_layer, ibv_link_layer_enums);
    printf ("\n");
}

/**
 * @brief Display the capabilities of a Queue Pair, for diagnostic information
 * @param[in] qp The Queue Pair to display the capabilities of
 * @param[in] qp_name String which identifies the Queue Pair
 */
void display_qp_capabilities (struct ibv_qp *const qp, const char *qp_name)
{
    int rc;
    struct ibv_qp_attr attr;
    struct ibv_qp_init_attr init_attr;

    rc = ibv_query_qp (qp, &attr, IBV_QP_STATE | IBV_QP_CAP, &init_attr);
    if (rc != 0)
    {
        perror ("ibv_query_qp failed");
        exit (EXIT_FAILURE);
    }

    printf ("%s capabilities: max_send_wr=%u max_recv_wr=%u max_send_sge=%u max_recv_sge=%u max_inline_data=%u\n",
            qp_name, attr.cap.max_send_wr, attr.cap.max_recv_wr, attr.cap.max_send_sge, attr.cap.max_recv_sge, attr.cap.max_inline_data);
}

/**
 * @brief Verify that the current state of a Queue Pair is the expected state, aborting on a difference
 * @param[in] expected_state The expected state of the Queue Pair
 * @param[in] qp The Queue Pair to check the state of
 * @param[in] qp_name String which identifies the Queue Pair in an error message
 */
void verify_qp_state (const enum ibv_qp_state expected_state, struct ibv_qp *const qp, const char *qp_name)
{
    int rc;
    struct ibv_qp_attr attr;
    struct ibv_qp_init_attr init_attr;

    rc = ibv_query_qp (qp, &attr, IBV_QP_STATE, &init_attr);
    if (rc != 0)
    {
        perror ("ibv_query_qp failed");
        exit (EXIT_FAILURE);
    }

    if (attr.qp_state != expected_state)
    {
        printf ("%s", qp_name);
        printf (" expected state=");
        display_enumeration_name (expected_state, ibv_qp_state_enums);
        printf (" actual state=");
        display_enumeration_name (attr.qp_state, ibv_qp_state_enums);
        printf ("\n");
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Return the maximum number of inline bytes which can be sent using a Queue Pair
 * @param[in] qp The Queue Pair to get the maximum number of inline bytes for
 * @return Returns the maximum number of lineline bytes for qp
 */
uint32_t get_max_inline_data (struct ibv_qp *const qp)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;

    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    memset (&qp_attr, 0, sizeof (qp_attr));
    rc = ibv_query_qp (qp, &qp_attr, IBV_QP_CAP, &qp_init_attr);
    if (rc != 0)
    {
        perror ("ibv_query_qp failed");
        exit (EXIT_FAILURE);
    }

    return qp_init_attr.cap.max_inline_data;
}

/**
 * @brief Obtain a pseudo-random 24-bit Packet Sequence Number
 * @return Returns the starting Packet Sequence Numb er
 */
uint32_t get_random_psn (void)
{
    return lrand48 () & 0xffffff;
}

/**
 * @brief Abort the program if an assertion check fails
 * @param[in] assertion If not true a programming error has been detected
 * @param[in] message Displayed if the assertion check fails
 */
void check_assert (const bool assertion, const char *message)
{
    if (!assertion)
    {
        printf ("Assertion failed: %s\n", message);
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Read the total number of interrupts which have been delivered to the Mellanox mlx4 driver.
 * @details This is used to determine if the Infiniband message passing tests cause interrupts to be generated.
 * @return Returns The total number of interrupts delivered to the Mellanox mlx4 driver.
 */
static uint64_t get_total_mlx4_interrupts (void)
{
    const char *whitespace_delim = " \t";
    bool results_valid = false;
    FILE *interrupts_file;
    uint64_t total_mlx4_interrupts;
    char line[1024];
    unsigned int num_cpus;
    int cpu;
    char *saveptr;
    char *token;
    unsigned long current_interrupt_id_total;
    bool total_valid;
    unsigned int cpu_index;
    unsigned long interrupt_count;
    char junk;

    total_mlx4_interrupts = 0;
    interrupts_file = fopen ("/proc/interrupts", "r");
    if (interrupts_file != NULL)
    {
        /* Parse the first line of /proc/interrupts to obtain the number of CPUs */
        num_cpus = 0;
        if (fgets (line, sizeof (line), interrupts_file) != NULL)
        {
            saveptr = NULL;
            token = strtok_r (line, whitespace_delim, &saveptr);
            while ((token != NULL) && (sscanf (token, "CPU%d", &cpu) == 1))
            {
                num_cpus++;
                token = strtok_r (NULL, whitespace_delim, &saveptr);
            }
        }

        if (num_cpus > 0)
        {
            while (fgets (line, sizeof (line), interrupts_file) != NULL)
            {
                /* Skip the interrupt ID */
                saveptr = NULL;
                token = strtok_r (line, whitespace_delim, &saveptr);

                /* Sum the number of interrupts delivered to all CPUs for this interrupt ID */
                current_interrupt_id_total = 0;
                total_valid = true;
                for (cpu_index = 0; total_valid && (cpu_index < num_cpus); cpu_index++)
                {
                    token = strtok_r (NULL, whitespace_delim, &saveptr);
                    total_valid = (token != NULL) && (sscanf (token, "%lu%c", &interrupt_count, &junk) == 1);
                    if (total_valid)
                    {
                        current_interrupt_id_total += interrupt_count;
                    }
                }

                /* saveptr is the remainder of the line after the interrupt counts; which contains multiple white space
                 * delimited fields containing a description of the interrupt and the attached driver. */
                if (total_valid && (strstr (saveptr, "mlx4") != NULL))
                {
                    /* This interrupt ID is serviced by the mlx4 driver */
                    total_mlx4_interrupts += current_interrupt_id_total;
                    results_valid = true;
                }
            }
        }
        fclose (interrupts_file);
    }

    if (!results_valid)
    {
        fprintf (stderr, "Failed to get count of mlx4 interrupts\n");
        exit (EXIT_FAILURE);
    }

    return total_mlx4_interrupts;
}

/**
 * @brief Obtain the Infiniband performance counter values for reporting statistics
 * @param[in] handle Used to obtain the Infiniband port statistics
 * @param[out] counters Where to store the performance counter values
 */
static void get_infiniband_performance_counters (const infiniband_statistics_handle *const handle,
                                                 infiniband_port_counters *const counters)
{
    uint8_t port_index;
    ib_portid_t portid;
    uint8_t mad_buf[1024];
    const int timeout = 0;

    for (port_index = 0; port_index < handle->num_ports; port_index++)
    {
        memset (&portid, 0, sizeof (portid));
        portid.lid = handle->port_lids[port_index];

        /* Determine which counters are available */
        memset (mad_buf, 0, sizeof (mad_buf));
        if (pma_query_via (mad_buf, &portid, handle->port_numbers[port_index], timeout,
                           CLASS_PORT_INFO, handle->mad_port) == NULL)
        {
            fprintf (stderr, "pma_query_via failed for %s LID %u\n", handle->hca, portid.lid);
            exit (EXIT_FAILURE);
        }
        mad_decode_field (mad_buf, IB_CPI_CAPMASK_F, &counters[port_index].cap_mask);
        counters[port_index].cap_mask = CL_HTON16 (counters[port_index].cap_mask);

        memset (mad_buf, 0, sizeof (mad_buf));
        if (pma_query_via (mad_buf, &portid, handle->port_numbers[port_index], timeout,
                           IB_GSI_PORT_COUNTERS_EXT, handle->mad_port) == NULL)
        {
            fprintf (stderr, "pma_query_via failed for %s LID %u\n", handle->hca, portid.lid);
            exit (EXIT_FAILURE);
        }

        mad_decode_field (mad_buf, IB_PC_EXT_XMT_BYTES_F, &counters[port_index].tx_bytes);
        mad_decode_field (mad_buf, IB_PC_EXT_RCV_BYTES_F, &counters[port_index].rx_bytes);
        mad_decode_field (mad_buf, IB_PC_EXT_XMT_PKTS_F, &counters[port_index].tx_packets);
        mad_decode_field (mad_buf, IB_PC_EXT_RCV_PKTS_F, &counters[port_index].rx_packets);
        mad_decode_field (mad_buf, IB_PC_EXT_XMT_UPKTS_F, &counters[port_index].unicast_tx_packets);
        mad_decode_field (mad_buf, IB_PC_EXT_RCV_UPKTS_F, &counters[port_index].unicast_rx_packets);
        mad_decode_field (mad_buf, IB_PC_EXT_XMT_MPKTS_F, &counters[port_index].multicast_tx_packets);
        mad_decode_field (mad_buf, IB_PC_EXT_RCV_MPKTS_F, &counters[port_index].multicast_rx_packets);


        memset (mad_buf, 0, sizeof (mad_buf));
        if (pma_query_via (mad_buf, &portid, handle->port_numbers[port_index], timeout,
                           IB_GSI_PORT_COUNTERS, handle->mad_port) == NULL)
        {
            fprintf (stderr, "pma_query_via failed for %s LID %u\n", handle->hca, portid.lid);
            exit (EXIT_FAILURE);
        }

        mad_decode_field (mad_buf, IB_PC_XMT_DISCARDS_F, &counters[port_index].xmit_discards);
        mad_decode_field (mad_buf, IB_PC_XMT_WAIT_F, &counters[port_index].xmit_wait);
    }
}

/**
 * @brief Sample the Infiniband statistics before a test
 * @param[in] handle Used to obtain the Infiniband port statistics
 * @param[in,out] stats Where to store the sampled statistics
 */
void get_infiniband_statistics_before_test (const infiniband_statistics_handle *const handle,
                                            infiniband_statistics_collection *const stats)
{
    int rc;

    get_infiniband_performance_counters (handle, stats->before.port_counters);
    stats->before.total_mlx4_interrupts = get_total_mlx4_interrupts ();

    /* Sample usage last since previous sample collection may have generated context switches */
    rc = getrusage (RUSAGE_SELF, &stats->before.usage);
    CHECK_ASSERT (rc == 0);
}

/**
 * @brief Sample the Infiniband statistics after a test
 * @param[in] handle Used to obtain the Infiniband port statistics
 * @param[in,out] stats Where to store the sampled statistics
 */
void get_infiniband_statistics_after_test (const infiniband_statistics_handle *const handle,
                                           infiniband_statistics_collection *const stats)
{
    int rc;

    /* Sample usage first since following sample collection may generate context switches */
    rc = getrusage (RUSAGE_SELF, &stats->after.usage);
    CHECK_ASSERT (rc == 0);

    stats->after.total_mlx4_interrupts = get_total_mlx4_interrupts ();
    get_infiniband_performance_counters (handle, stats->after.port_counters);
}

/**
 * @brief Open a handle to be used to obtain Infiniband statistics for the ports on one Infiniband device
 * @param[out] handle The handle which has been opened
 * @param[in] device The Infiniband device for which to obtain the statistics for
 * @param[in] device_attr The attributes of the Infiniband device for which to obtain the statistics for
 * @param[in] port_attrs The attributes for the ports of the Infiniband for which to obtain the statistics for.
 *                       The port indices count from one.
 */
void open_infiniband_statistics_handle (infiniband_statistics_handle *const handle,
                                        const struct ibv_context *const device, const struct ibv_device_attr *const device_attr,
                                        const struct ibv_port_attr *const port_attrs)
{
    uint8_t port;
    int mgmt_class = IB_PERFORMANCE_CLASS;

    /* Store the port LIDs for use when collecting the port statistics */
    handle->num_ports = 0;
    for (port = 1; (port <= device_attr->phys_port_cnt) && (handle->num_ports < MAX_STATISTICS_PORTS); port++)
    {
        handle->port_lids[handle->num_ports] = port_attrs[port].lid;
        handle->port_numbers[handle->num_ports] = port;
        handle->num_ports++;
    }

    /* Open a MAD port using the first port of the Infiniband device, which can be used to collect statistics for all ports */
    port = 1;
    handle->hca = strdup (ibv_get_device_name (device->device));
    handle->mad_port = mad_rpc_open_port (handle->hca, port, &mgmt_class, 1);
    if (handle->mad_port == NULL)
    {
        fprintf (stderr, "mad_rpc_open_port failed for %s port %u\n", handle->hca, port);
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Close the handle used for obtaining Infiniband statistics
 * @param[in,out] handle The handle to close
 */
void close_infiniband_statistics_handle (infiniband_statistics_handle *const handle)
{
    mad_rpc_close_port (handle->mad_port);
    free (handle->hca);
    handle->mad_port = NULL;
    handle->hca = NULL;
    handle->num_ports = 0;
}

/**
 * @brief Display the change in the Infiniband statistics during a test
 * @param[in] stats Contains the sampled statistics to display
 * @param[in] description Output as a description of the test for which the statistics apply
 */
void display_infiniband_statistics (infiniband_statistics_handle *const handle,
                                    const infiniband_statistics_collection *const stats, const char *description)
{
    uint8_t port_index;

    printf ("Changes to statistics for %s:\n", description);
    printf ("  Num voluntary context switches = %ld (%ld -> %ld)\n",
            stats->after.usage.ru_nvcsw - stats->before.usage.ru_nvcsw,
            stats->before.usage.ru_nvcsw, stats->after.usage.ru_nvcsw);
    printf ("  Num page reclaims = %ld (%ld -> %ld)\n",
            stats->after.usage.ru_minflt - stats->before.usage.ru_minflt,
            stats->before.usage.ru_minflt, stats->after.usage.ru_minflt);
    printf ("  Num page faults = %ld (%ld -> %ld)\n",
            stats->after.usage.ru_majflt - stats->before.usage.ru_majflt,
            stats->before.usage.ru_majflt, stats->after.usage.ru_majflt);
    printf ("  Num mlx4 interrupts = %lu (%lu -> %lu)\n",
            stats->after.total_mlx4_interrupts - stats->before.total_mlx4_interrupts,
            stats->before.total_mlx4_interrupts, stats->after.total_mlx4_interrupts);

    for (port_index = 0; port_index < handle->num_ports; port_index++)
    {
        const infiniband_port_counters *const before = &stats->before.port_counters[port_index];
        const infiniband_port_counters *const after = &stats->after.port_counters[port_index];
        const uint8_t port = handle->port_numbers[port_index];

        if ((before->cap_mask & IB_PM_EXT_WIDTH_SUPPORTED) || (before->cap_mask & IB_PM_EXT_WIDTH_NOIETF_SUP))
        {
            printf ("  Port %u Tx Bytes = %lu (%lu -> %lu)\n", port,
                    after->tx_bytes - before->tx_bytes, before->tx_bytes, after->tx_bytes);
            printf ("  Port %u Rx Bytes = %lu (%lu -> %lu)\n", port,
                    after->rx_bytes - before->rx_bytes, before->rx_bytes, after->rx_bytes);
            printf ("  Port %u Tx Packets = %lu (%lu -> %lu)\n", port,
                    after->tx_packets - before->tx_packets, before->tx_packets, after->tx_packets);
            printf ("  Port %u Rx Packets = %lu (%lu -> %lu)\n", port,
                    after->rx_packets - before->rx_packets, before->rx_packets, after->rx_packets);
            if (before->cap_mask & IB_PM_EXT_WIDTH_SUPPORTED)
            {
                printf ("  Port %u Tx Unicast Packets = %lu (%lu -> %lu)\n", port,
                        after->unicast_tx_packets - before->unicast_tx_packets, before->unicast_tx_packets, after->unicast_tx_packets);
                printf ("  Port %u Rx Unicast Packets = %lu (%lu -> %lu)\n", port,
                        after->unicast_rx_packets - before->unicast_rx_packets, before->unicast_rx_packets, after->unicast_rx_packets);
                printf ("  Port %u Tx Multicast Packets = %lu (%lu -> %lu)\n", port,
                        after->multicast_tx_packets - before->multicast_tx_packets, before->multicast_tx_packets, after->multicast_tx_packets);
                printf ("  Port %u Rx Multicast Packets = %lu (%lu -> %lu)\n", port,
                        after->multicast_rx_packets - before->multicast_rx_packets, before->multicast_rx_packets, after->multicast_rx_packets);
            }
        }

        printf ("  Port %u Tx Discards = %u (%u -> %u)\n", port,
                after->xmit_discards - before->xmit_discards, before->xmit_discards, after->xmit_discards);
        if (before->cap_mask & IB_PM_PC_XMIT_WAIT_SUP)
        {
            printf ("  Port %u Tx Waits = %u (%u -> %u)\n", port,
                    after->xmit_wait - before->xmit_wait, before->xmit_wait, after->xmit_wait);
        }
    }
    printf ("\n");
}

/**
 * @brief Display the current CPU frequencies to standard out
 */
void display_current_cpu_frequencies (void)
{
    const int num_cpus = sysconf (_SC_NPROCESSORS_ONLN);
    int cpu_index;
    FILE *cpu_freq_file;
    char cpu_freq_pathname[PATH_MAX];
    int cpu_freq_mhz;

    printf ("Current CPU frequencies (MHz) :");
    for (cpu_index = 0; cpu_index < num_cpus; cpu_index++)
    {
        bool read_cpu_freq = false;

        sprintf (cpu_freq_pathname, "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_cur_freq", cpu_index);
        cpu_freq_file = fopen (cpu_freq_pathname, "r");
        if (cpu_freq_file != NULL)
        {
            read_cpu_freq = fscanf (cpu_freq_file, "%d\n", &cpu_freq_mhz) == 1;
            fclose (cpu_freq_file);
        }

        if (read_cpu_freq)
        {
            printf ("  %d", cpu_freq_mhz);
        }
        else
        {
            printf ("  ???");
        }
    }
    printf ("\n");
}
