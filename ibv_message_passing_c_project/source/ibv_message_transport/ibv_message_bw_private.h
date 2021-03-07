/*
 * @file ibv_message_bw_private.h
 * @date 7 Mar 2021
 * @author Chester Gillon
 * @brief Contains the public interface for the ibv_message_transport library
 */

#ifndef IBV_MESSAGE_BW_PRIVATE_H_
#define IBV_MESSAGE_BW_PRIVATE_H_

/** SLP service name used to publish a transmit or receive endpoint for a communication path */
#define SLP_SERVICE_NAME "service:message_bw"


/** The attributes of a memory buffer which are exchanged by SLP, and used to establish an Infiniband connection */
typedef struct
{
    /** The size of the memory buffer in bytes */
    size_t size;
    /** The virtual address of the memory buffer */
    uint64_t addr;
    /** The key to access the memory buffer */
    uint32_t rkey;
    /** The LID to address the memory buffer */
    uint16_t lid;
    /** The initial packet-sequence number */
    uint32_t psn;
    /** The Queue Pair number for the memory buffer */
    uint32_t qp_num;
    /** true once the Queue Pair is ready to receive */
    int qp_ready_to_receive;
    /** The GID index to address the memory buffer when using RoCE. -1 means a global address isn't used */
    int gid_index;
    /** The GID used to address the memory buffer when using RoCE */
    union ibv_gid gid;
} memory_buffer_attributes;

/** Contains the context to use SLP to publish the local Queue Pair information for a communication path, and then obtain
 *  the remote Queue Pair information.
 *  Since SLP is used, the IP address of the remote end is not required. */
#define SLP_SERVICE_URL_MAX_LEN 1024
#define SLP_ATTRIBUTES_MAX_LEN  1024
typedef struct
{
    /** Handle used to perform SLP operations */
    SLPHandle handle;
    /** The SLP service URL for the local endpoint of the communication path which is published with the local Queue Pair information */
    char local_service_url[SLP_SERVICE_URL_MAX_LEN];
    /** The name portion of the SLP service URL for the remote endpoint of the communication path
     *  from which the remote Queue Pair information is obtained.
     *  Only the name portion is given as the hostname component is not known at initialisation. */
    char remote_service_name[SLP_SERVICE_URL_MAX_LEN];
    /** The SLP service URL for the remote endpoint of the communication path, set by searching for remote_service_name
     *  in all available service URLs. */
    char remote_service_url[SLP_SERVICE_URL_MAX_LEN];
    /** Set once have retrieved the remote_attributes */
    bool remote_attributes_obtained;
    /** The attributes published for the local memory buffer */
    memory_buffer_attributes local_attributes;
    /** The attributes retrieved for the remote memory buffer */
    memory_buffer_attributes remote_attributes;
} communication_path_slp_connection;

/** Contains the Infiniband device and port on the local host which is used for transmitting or receiving messages from */
typedef struct
{
    /** The number of devices on the local host */
    int num_devices;
    /** The list of all devices on the local host */
    struct ibv_device **device_list;
    /** Which device in the device_list is used for transmitting or receiving messages from */
    struct ibv_device *selected_device;
    /** The opened context for selected_device */
    struct ibv_context *device_context;
    /** The protection domain for the device_context */
    struct ibv_pd *device_pd;
    /** The attributes for selected_device */
    struct ibv_device_attr device_attributes;
    /** The number of the selected port */
    uint8_t port_num;
    /** The attributes for the port on selected_device which is used for transmitting or receiving messages from */
    struct ibv_port_attr port_attributes;
} ib_port_endpoint;

/** Contains a memory buffer which is used for transmitting or receiving messages from the Infiniband device */
typedef struct
{
    /** How the memory for the buffer is allocated */
    buffer_allocation_type allocation_type;
    /** The size of the buffer in bytes */
    size_t size;
    /** The allocated buffer */
    char *buffer;
    /** For BUFFER_ALLOCATION_SHARED_MEMORY the name of the POSIX shared memory file */
    char pathname[PATH_MAX];
    /** For BUFFER_ALLOCATION_SHARED_MEMORY the file descriptor of the POSIX shared memory file */
    int fd;
} memory_buffer;

void open_ib_port_endpoint (ib_port_endpoint *const endpoint, const communication_path_definition *const path_def,
                            const bool is_tx_end);
void close_ib_port_endpoint (ib_port_endpoint *const endpoint);
void intialise_slp_connection (communication_path_slp_connection *const slp_connection, const bool is_tx_end,
                               const communication_path_definition *const path_def);
void register_memory_buffer_with_slp (communication_path_slp_connection *const slp_connection,
                                      const ib_port_endpoint *const endpoint, const uint32_t psn,
                                      const struct ibv_mr *const mr, const struct ibv_qp *const qp);
void report_local_memory_buffer_rtr_with_slp (communication_path_slp_connection *const slp_connection);
void get_remote_memory_buffer_from_slp (communication_path_slp_connection *const slp_connection);
void await_remote_memory_buffer_rtr_from_slp (communication_path_slp_connection *const slp_connection);
void close_slp_connection (communication_path_slp_connection *const slp_connection);
void create_memory_buffer (memory_buffer *const buffer, const bool is_tx_end,
                           const communication_path_definition *const path_def);
void release_memory_buffer (memory_buffer *const buffer);
uint32_t get_random_psn (void);
uint32_t get_max_inline_data (struct ibv_qp *const qp);
void verify_qp_state (const enum ibv_qp_state expected_state, struct ibv_qp *const qp, const char *qp_name,
                      const communication_path_definition *const path_def);

#endif /* IBV_MESSAGE_BW_PRIVATE_H_ */
