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

void *page_aligned_alloc (const size_t size);
void test_sender_rdma_write_receiver_passive (void);

#endif /* IBV_FUNCTIONAL_LOOPBACK_TEST_INTERFACE_H_ */
