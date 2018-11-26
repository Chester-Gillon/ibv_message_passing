/*
 * @file ibv_utils.h
 * @date: 9 Sep 2017
 * @author Chester Gillon
 * @brief Utility functions for Infiniband tests
 */

#ifndef IBV_UTILS_H_
#define IBV_UTILS_H_

/** Contains the performance counters for one Infiniband port.
 *  These are read as extended 64-bit counts where possible. */
typedef struct
{
    uint32_t cap_mask;
    uint64_t tx_bytes;
    uint64_t rx_bytes;
    uint64_t tx_packets;
    uint64_t rx_packets;
    uint64_t unicast_tx_packets;
    uint64_t unicast_rx_packets;
    uint64_t multicast_tx_packets;
    uint64_t multicast_rx_packets;
    uint32_t xmit_discards;
    uint32_t xmit_wait;
} infiniband_port_counters;

/** Contains one sample of Infiniband related statistics */
#define MAX_STATISTICS_PORTS 2
typedef struct
{
    /** Usage information for the calling process, to determine if context switches occur */
    struct rusage usage;
    /** The total number of interrupts which have been delivered to the Mellanox mlx4 driver */
    uint64_t total_mlx4_interrupts;
    /** The statistics from the Infiniband ports */
    infiniband_port_counters port_counters[MAX_STATISTICS_PORTS];
} infiniband_statistics_sample;

/** Contains Infiniband related statistics before and after a test, to report changes in the statistics values */
typedef struct
{
    /** Statistics sampled before the test */
    infiniband_statistics_sample before;
    /** Statistics sampled after the test */
    infiniband_statistics_sample after;
} infiniband_statistics_collection;

/** Contains the fields to be used to obtain statistics for Infiniband ports */
typedef struct
{
    /** The number of Infiniband ports for which statistics are collected */
    unsigned int num_ports;
    /** The list of LIDs of the ports for which statistics are collected */
    uint16_t port_lids[MAX_STATISTICS_PORTS];
    /** The number of the ports for which statistics are collected */
    uint8_t port_numbers[MAX_STATISTICS_PORTS];
    /** The name of the HCA used to collect the statistics for */
    char *hca;
    /** The ibmad port used to collect the port statistics */
    struct ibmad_port *mad_port;
} infiniband_statistics_handle;

void *page_aligned_alloc (const size_t size);
void *page_aligned_calloc (const size_t nmemb, const size_t size);
void *cache_line_aligned_alloc (const size_t size);
void *cache_line_aligned_calloc (const size_t nmemb, const size_t size);
void display_ibv_device_attributes (const struct ibv_context *const device, const struct ibv_device_attr *const device_attr);
void display_ibv_port_attributes (const struct ibv_port_attr *const port_attr);
void display_qp_capabilities (struct ibv_qp *const qp, const char *qp_name);
void verify_qp_state (const enum ibv_qp_state expected_state, struct ibv_qp *const qp, const char *qp_name);
uint32_t get_max_inline_data (struct ibv_qp *const qp);
uint32_t get_random_psn (void);
void get_infiniband_statistics_before_test (const infiniband_statistics_handle *const handle,
                                            infiniband_statistics_collection *const stats);
void get_infiniband_statistics_after_test (const infiniband_statistics_handle *const handle,
                                           infiniband_statistics_collection *const stats);
void open_infiniband_statistics_handle (infiniband_statistics_handle *const handle,
                                        const struct ibv_context *const device, const struct ibv_device_attr *const device_attr,
                                        const struct ibv_port_attr *const port_attrs);
void close_infiniband_statistics_handle (infiniband_statistics_handle *const handle);
void display_infiniband_statistics (infiniband_statistics_handle *const handle,
                                    const infiniband_statistics_collection *const stats, const char *description);
void display_current_cpu_frequencies (void);
void check_assert (const bool assertion, const char *message);
#define CHECK_ASSERT(assertion) check_assert(assertion,#assertion)

/** The assumed cache line size for allocating areas. Should be valid for all Sandy Bridge and Haswell processors */
#define CACHE_LINE_SIZE_BYTES 64

#endif /* IBV_UTILS_H_ */
