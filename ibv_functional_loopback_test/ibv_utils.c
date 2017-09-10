/*
 * @file ibv_utils.c
 * @date: 9 Sep 2017
 * @author Chester Gillon
 * @brief Utility functions for Infiniband tests
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>

#include <infiniband/verbs.h>

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
 *  @todo This list of enumerations was taken from the verbs.h installed from libibverbs-dev_1.1.8-1.1ubuntu2_amd64.deb
 *        With a Mellanox Technologies MT26428 the following bits are set in device_cap_flags for which there is no enumeration:
 *        - BIT15 - IB_DEVICE_LOCAL_DMA_LKEY in some versions of ib_verbs.h
 *        - BIT19 - IB_DEVICE_UD_IP_CSUM is some versions of ib_verbs.h
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
    ENUM_LIST_INIT (IBV_DEVICE_IP_CSUM),
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
    printf (" node_guid=0x%lx sys_image_guid=0x%lx\n", device_attr->node_guid, device_attr->sys_image_guid);
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

    rc = ibv_query_qp (qp, &attr, IBV_QP_STATE, &init_attr);
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
 * @brief Obtain a pseudo-random 24-bit Packet Sequence Number
 * @return Returns the starting Packet Sequence Numb er
 */
uint32_t get_random_psn (void)
{
    return lrand48 () & 0xffffff;
}
