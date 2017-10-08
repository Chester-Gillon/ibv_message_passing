/*
 * @file ibv_message_bw_interface.h
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains the interface between the source files of the ibv_message_bw program
 */

#ifndef IBV_MESSAGE_BW_INTERFACE_H_
#define IBV_MESSAGE_BW_INTERFACE_H_

/** Defines a communication path for which one ibv_message_bw program is the transmit or receive endpoint */
typedef struct
{
    /** The numeric instance which the transmit and receive endpoints used to identify the communication path.
     *  This is used as part of a SLP service URL which is used to publish the information for the endpoints
     *  to connect the Queue Pairs.
     *
     *  Since there is flow-control sent from the message receiver back to the message sender,
     *  both endpoints need to connect to each other. */
    int instance;
    /** The name of the Infiniband device used to transmit or receive messages */
    const char *ib_device;
    /** The port of the Infiniband device used to transmit or receive messages */
    uint8_t port_num;
    /** The maximum message data size sent on the communication path */
    uint32_t max_message_size;
    /** The number of message buffers on the communication path */
    uint32_t num_message_buffers;
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
    /** The attributes for selected_device */
    struct ibv_device_attr device_attributes;
    /** The attributes for the port on selected_device which is used for transmitting or receiving messages from */
    struct ibv_port_attr port_attributes;
} ib_port_endpoint;

/** Contains the context to use SLP to publish the local Queue Pair information for a communication path, and then obtain
 *  the remote Queue Pair information.
 *  Since SLP is used, the IP address of the remote end is not required. */
#define SERVICE_URL_MAX_LEN 1024
typedef struct
{
    /** Handle used to perform SLP operations */
    SLPHandle handle;
    /** The SLP service URL for the local endpoint of the communication path which is published with the local Queue Pair information */
    char local_service_url[SERVICE_URL_MAX_LEN];
    /** The SLP service URL for the remove endpoint of the communication path from which the remote Queue Pair information is obtained */
    char remote_service_url[SERVICE_URL_MAX_LEN];
} communication_path_slp_connection;

/** Opaque handles for the context used to transmit or receive messages on a communication path */
struct tx_message_context_s;
typedef struct tx_message_context_s *tx_message_context_handle;

struct rx_message_context_s;
typedef struct rx_message_context_s *rx_message_context_handle;

void check_assert (const bool assertion, const char *message);
#define CHECK_ASSERT(assertion) check_assert(assertion,#assertion)
void *page_aligned_alloc (const size_t size);
void *page_aligned_calloc (const size_t nmemb, const size_t size);
void *cache_line_aligned_alloc (const size_t size);
void *cache_line_aligned_calloc (const size_t nmemb, const size_t size);
void open_ib_port_endpoint (ib_port_endpoint *const endpoint, const communication_path_definition *const path_def);
void close_ib_port_endpoint (ib_port_endpoint *const endpoint);
void intialise_slp_connection (communication_path_slp_connection *const slp_connection, const bool is_tx_end,
                               const communication_path_definition *const path_def);
void close_slp_connection (communication_path_slp_connection *const slp_connection);

tx_message_context_handle message_transmit_create_local (const communication_path_definition *const path_def);
void message_transmit_attach_remote (tx_message_context_handle context);
void message_transmit_finalise (tx_message_context_handle context);

rx_message_context_handle message_receive_create_local (const communication_path_definition *const path_def);
void message_receive_attach_remote (rx_message_context_handle context);
void message_receive_finalise (rx_message_context_handle context);

/** The assumed cache line size for allocating areas. Should be valid for all Sandy Bridge and Haswell processors */
#define CACHE_LINE_SIZE_BYTES 64

#endif /* IBV_MESSAGE_BW_INTERFACE_H_ */
