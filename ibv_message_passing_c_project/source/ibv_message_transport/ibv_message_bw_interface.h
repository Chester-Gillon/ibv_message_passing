/*
 * @file ibv_message_bw_interface.h
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains the interface for the ibv_message_transport library
 */

#ifndef IBV_MESSAGE_BW_INTERFACE_H_
#define IBV_MESSAGE_BW_INTERFACE_H_

/** SLP service name used to publish a transmit or receive endpoint for a communication path */
#define SLP_SERVICE_NAME "service:message_bw"

/** No need to set a specific Infiniband service level for the tests */
#define DEFAULT_SERVICE_LEVEL 0

/** Defines the options used to allocate a buffer which is used to transmit or receive messages.
 *  This buffer will be registered as a memory region with the Infiniband device */
typedef enum
{
    /** Allocate the buffer from the heap of the calling process */
    BUFFER_ALLOCATION_HEAP,
    /** Allocate the buffer from POSIX shared memory */
    BUFFER_ALLOCATION_SHARED_MEMORY
} buffer_allocation_type;

/** Defines a communication path for which one ibv_message_bw program is the transmit or receive endpoint */
typedef struct
{
    /** The source node endpoint identity */
    int source_node;
    /** The destination endpoint identity */
    int destination_node;
    /** The numeric instance which the transmit and receive endpoints used to identify the communication path.
     *  The source_node, destination_node and instance are used as part of a SLP service URL which is used to
     *  publish the information for the endpoints to connect the Queue Pairs.
     *
     *  Since there is flow-control sent from the message receiver back to the message sender,
     *  both endpoints need to connect to each other. */
    int instance;
    /** The name of the Infiniband device used by the source endpoint to transmit messages */
    const char *source_ib_device;
    /** The name of the Infiniband device used by the destination endpoint to receive messages */
    const char *destination_ib_device;
    /** The port of the Infiniband device used by the source endpoint to receive messages */
    uint8_t source_port_num;
    /** The port of the Infiniband device used by the destination endpoint to receive messages */
    uint8_t destination_port_num;
    /** The Infiniband Service Level used on the communication path */
    uint8_t service_level;
    /** The maximum message data size sent on the communication path */
    uint32_t max_message_size;
    /** The number of message buffers on the communication path */
    uint32_t num_message_buffers;
    /** How the transmit or receive buffer is allocated */
    buffer_allocation_type allocation_type;
    /** When true the transmitter polls for Infiniband errors while waiting to obtain a send buffer.
     *  When false if the receiver process terminates abnormally the transmitter will just block once all messages buffers
     *  are in use. */
    bool tx_polls_for_errors;
    /** When true the transmitter checks the size of the memory buffer on the transmitter and receiver is the same
     *  before starting to send messages, abort with an error if the sizes doesn't match (meaning the path definition is
     *  not the same on the transmitter and receiver).
     *
     *  When false the size of the memory buffer is not checked, and if the size is different the transmitter may get
     *  an Infiniband error if the memory buffer size on the transmitter is larger. */
    bool tx_checks_memory_buffer_size;
    /** When true set the Infiniband transport retry timeout using the value of retry_timeout.
     *  When false set to the local_ca_ack_delay for the device. */
    bool set_non_default_retry_timeout;
    /** The Infiniband transport retry timeout, in units of 4.096*2^timeout microseconds */
    uint8_t retry_timeout;
} communication_path_definition;

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

/** The test application header placed at the start of every message sent */
typedef struct
{
    /** An incrementing sequence number for every message sent */
    uint32_t sequence_number;
    /** The number of data bytes in the message. Maximum message length is part of the communication path definition. */
    uint32_t message_length;
    /** The identity of the message */
    uint32_t message_id;
    /** The source instance of the message, used as an arbitrary value for these tests */
    uint32_t source_instance;
} message_header;

/** Opaque handles for the context used to transmit or receive messages on a communication path */
struct tx_message_context_s;
typedef struct tx_message_context_s *tx_message_context_handle;

struct rx_message_context_s;
typedef struct rx_message_context_s *rx_message_context_handle;

/** Defines one message buffer which is sent by the test application.
 *  The test application de-references the header and data pointers to populate the message to send.
 *  The send API functions are responsible for setting the pointers and the buffer_index */
typedef struct
{
    /** Points at the header of the message which is to be sent */
    message_header *header;
    /** Points at the data of the message which is to be sent */
    void *data;
    /** The transmit context to send the message on */
    tx_message_context_handle context;
    /** The message index into the circular buffer of messages */
    uint32_t buffer_index;
} tx_api_message_buffer;

/** Defines one message buffer which is received by the test application.
 *  The test application de-references the header and data pointers to read the received message.
 *  The receive API functions are responsible for setting the pointers and the buffer_index */
typedef struct
{
    /** Points at the header of the message which has been received */
    message_header *header;
    /** Points at the data of the message which has been received */
    void *data;
    /** The receive context the message has been received from */
    rx_message_context_handle context;
    /** The message index into the circular buffer of messages */
    uint32_t buffer_index;
} rx_api_message_buffer;

/** Opaque handle for the context used to transmit or receive messages on multiple communication paths for one node */
struct communication_context_s;
typedef struct communication_context_s *communication_context_handle;

void check_assert (const bool assertion, const char *message);
#define CHECK_ASSERT(assertion) check_assert(assertion,#assertion)
void *page_aligned_alloc (const size_t size);
void *page_aligned_calloc (const size_t nmemb, const size_t size);
void *cache_line_aligned_alloc (const size_t size);
void *cache_line_aligned_calloc (const size_t nmemb, const size_t size);
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
size_t align_to_cache_line_size (const size_t size);
void create_memory_buffer (memory_buffer *const buffer, const bool is_tx_end,
                           const communication_path_definition *const path_def);
void release_memory_buffer (memory_buffer *const buffer);
uint32_t get_random_psn (void);
uint32_t get_max_inline_data (struct ibv_qp *const qp);
void verify_qp_state (const enum ibv_qp_state expected_state, struct ibv_qp *const qp, const char *qp_name,
                      const communication_path_definition *const path_def);

tx_message_context_handle message_transmit_create_local (const communication_path_definition *const path_def);
void message_transmit_attach_remote_pre_rtr (tx_message_context_handle context);
void message_transmit_attach_remote_post_rtr (tx_message_context_handle context);
void message_transmit_finalise (tx_message_context_handle context);
void await_all_outstanding_messages_freed (tx_message_context_handle context);
tx_api_message_buffer *get_send_buffer_no_wait (tx_message_context_handle context);
tx_api_message_buffer *get_send_buffer (tx_message_context_handle context);
void send_message (const tx_api_message_buffer *const api_buffer);
void flush_transmit_dma (tx_message_context_handle context);

rx_message_context_handle message_receive_create_local (const communication_path_definition *const path_def);
void message_receive_attach_remote_pre_rtr (rx_message_context_handle context);
void message_receive_attach_remote_post_rtr (rx_message_context_handle context);
void message_receive_finalise (rx_message_context_handle context);
rx_api_message_buffer *poll_rx_message (rx_message_context_handle context);
rx_api_message_buffer *await_message (rx_message_context_handle context);
void free_message (rx_api_message_buffer *const api_buffer);

void register_path_definition (const communication_path_definition *const path_def);
communication_context_handle communication_context_initialise (const int node_number);
tx_message_context_handle get_tx_path_handle (communication_context_handle context,
                                              const int destination_node, const int instance);
rx_api_message_buffer *poll_rx_paths (communication_context_handle context);
rx_api_message_buffer *await_any_rx_message (communication_context_handle context);
void communication_context_finalise (communication_context_handle context);

/** The assumed cache line size for allocating areas. Should be valid for all Sandy Bridge and Haswell processors */
#define CACHE_LINE_SIZE_BYTES 64

#endif /* IBV_MESSAGE_BW_INTERFACE_H_ */
