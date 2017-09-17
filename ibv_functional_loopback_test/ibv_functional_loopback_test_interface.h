/*
 * @file ibv_functional_loopback_test_interface.h
 * @date 3 Sep 2017
 * @author Chester Gillon
 * @brief Defines the interface between the source files of the ibv_functional_loopback_test program
 * @details There is a separate source file which implements each tested message mode
 */

#ifndef IBV_FUNCTIONAL_LOOPBACK_TEST_INTERFACE_H_
#define IBV_FUNCTIONAL_LOOPBACK_TEST_INTERFACE_H_

/** The Infiniband device used for the loopback tests, which is the first device found */
extern struct ibv_context *ibv_loopback_device;
extern struct ibv_device_attr ibv_loopback_device_attributes;

/** The ports used on the Infiniband device used for the loopback tests */
#define SOURCE_PORT_NUM      1
#define DESTINATION_PORT_NUM 2
#define NUM_TEST_PORTS       2
extern struct ibv_port_attr ibv_loopback_port_attributes[NUM_TEST_PORTS+1];

/** The protection domain for the Infiniband device used for the loopback tests */
extern struct ibv_pd *ibv_loopback_device_pd;

/** The maximum number of data bytes in a message, which doesn't include the message_header */
#define MAX_MESSAGE_DATA_LEN_BYTES (1 << 24)

/** No need to set a specific Infiniband service level for the tests */
#define DEFAULT_SERVICE_LEVEL 0

/** The test application header placed at the start of every message sent */
typedef struct
{
    /** An incrementing sequence number for every message sent */
    uint32_t sequence_number;
    /** The number of data bytes in the message. Range is 0..MAX_MESSAGE_DATA_LEN_BYTES */
    uint32_t message_length;
    /** The identity of the message, used as an arbitrary value for these tests */
    uint32_t message_id;
    /** The source instance of the message, used as an arbitrary value for these tests */
    uint32_t source_instance;
} message_header;

/** Structure used for each test variable length test message sent */
typedef struct
{
    /** The header at the start of the application message */
    message_header header;
    /** Variable size data of the message */
    uint32_t data[MAX_MESSAGE_DATA_LEN_BYTES / sizeof (uint32_t)];
} test_message;

/** The number of message which can be stored in a circular buffer between the sender and receiver.
 *  In conjunction with MAX_MESSAGE_DATA_LEN_BYTES this requires approximately 512 Mbytes for the transmit and receive buffers. */
#define NUM_MESSAGE_BUFFERS 32

/** Defines one message buffer which sent/received by the test application.
 *  The test application de-references the message pointer.
 *  The send/receive API functions are responsible for setting the message pointer and the buffer_index */
typedef struct
{
    /** Points at the message content which is to be sent or has been received */
    test_message *message;
    /** The message index into the circular buffer of messages */
    uint32_t buffer_index;
} api_message_buffer;

/** Opaque pointers for send and receive contexts used in message communication APIs to get type checking
 *  without details of the actual implementation method. */
typedef struct send_context_void *api_send_context;
typedef struct receive_context_void *api_receive_context;

/** Contains a set of function pointers used to transfer test messages for a given implementation method. */
typedef struct
{
    /** Describes communication method used by the function pointers */
    const char *description;

    /**
     * @brief Allocate and initialise the send and receive contexts to be used to transfer test messages
     * @param[out] send_context_out The allocated send context
     * @param[out] receive_context_out The allocated receive context
     */
    void (*initialise) (api_send_context *const send_context_out, api_receive_context *const receive_context_out);

    /**
     * @brief Free the sources used for the send and receive contexts used to transfer test messages
     * @param[in] send_context_in The send context to free the resources for
     * @param[in] receive_context_in The receive context to free the resources for
     */
    void (*finalise) (api_send_context send_context_in, api_receive_context receive_context_in);

    /**
     * @brief Get a message send buffer to populate
     * @details If the next message buffer is still in use, then waits with a busy-poll for the receiver to indicate
     *          the message has been freed (which also implies the previous send has completed).
     * @param[in,out] send_context_in The send context to get the message buffer for
     * @return Returns a pointer to a message buffer, for which the message field can be populated with a message to send
     */
    api_message_buffer *(*get_send_buffer) (api_send_context send_context_in);

    /**
     * @brief Send a message with the content which has been populated by the caller
     * @details When this function returns the message has been queued for transmission
     * @param[in,out] send_context_in The send context to send the message on
     * @param[in,out] buffer The message buffer to send, which was previously returned by get_send_buffer()
     *                       and which the message to send has been populated by the caller.
     *                       This function will set the buffer->message.header.sequence_number field
     */
    void (*send_message) (api_send_context send_context_in, api_message_buffer *const buffer);

    /**
     * @brief Wait for a message to be received, using a busy-poll
     * @details The contents of the received message remains valid until the returned buffer is freed by a
     *          call to free_message(). Flow control prevents a received message from being overwritten until freed
     * @param[in,out] receive_context_in The receive context to receive the message on
     * @return Returns a pointer to the received message buffer
     */
    api_message_buffer *(*await_message) (api_receive_context receive_context_in);

    /**
     * @brief Mark a receive message buffer after the received message has been freed
     * @details This implements flow control by indicating to sender that the buffer is now free for another message
     * @param[in,out] receive_context_in The receive context to free the message for
     * @param[in,out] buffer The received message buffer to free
     */
    void (*free_message) (api_receive_context receive_context_in, api_message_buffer *const buffer);
} message_communication_functions;

void sender_rdma_write_receiver_passive_set_functions (message_communication_functions *const functions);

#endif /* IBV_FUNCTIONAL_LOOPBACK_TEST_INTERFACE_H_ */
