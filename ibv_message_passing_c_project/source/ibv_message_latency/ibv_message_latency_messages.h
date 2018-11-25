/*
 * @file ibv_message_latency_messages.h
 * @date 18 Nov 2018
 * @author Chester Gillon
 * @brief Defines the messages exchanged between the message latency master and slave
 */

#ifndef SOURCE_IBV_MESSAGE_LATENCY_IBV_MESSAGE_LATENCY_MESSAGES_H_
#define SOURCE_IBV_MESSAGE_LATENCY_IBV_MESSAGE_LATENCY_MESSAGES_H_


/* The node identities for the master and slave */
typedef enum
{
    MESSAGE_LATENCY_MASTER_NODE_ID,
    MESSAGE_LATENCY_SLAVE_NODE_ID
} message_latency_node_ids;


/* The maximum number of parallel communication paths used by the program, set to the number of
 * Infiniband service levels. */
#define MESSAGE_LATENCY_MAX_PARALLEL_PATHS 15

/** The instances of different paths between the master and slave */
typedef enum
{
    /* The parallel paths from the master to slave over which messages are sent to measure latency */
    MASTER_TEST_MESSAGES_FIRST_PATH,
    /* The path from slave to master which contains the slave time of receipt for the messages on each
     * parallel path from the master. */
    SLAVE_ACK_PATH = MASTER_TEST_MESSAGES_FIRST_PATH + MESSAGE_LATENCY_MAX_PARALLEL_PATHS
} message_latency_path_instances;


/** The message IDs exchanged between the master and slave */
typedef enum
{
    /* Messages sent from master to slave to measure latency for */
    MESSAGE_LATENCY_DATA_MSG_ID,
    /* Sent from slave to master to give the time of receipt for all the latency measurement messages */
    MESSAGE_LATENCY_ACK_MSG_ID,
    /* Header only message sent from master to slave to indicate the test is complete and the slave is to exit */
    MESSAGE_LATENCY_TEST_COMPLETE_MSG_ID
} message_latency_msg_ids;


/* Used for the MESSAGE_LATENCY_ACK_MSG_ID */
typedef struct
{
    /* CLOCK_REALTIME when each message on the parallel path was received */
    struct timespec message_rx_times[MESSAGE_LATENCY_MAX_PARALLEL_PATHS];
} latency_ack_msg;


/* The size of size of the messages sent for measuring latency */
#define MESSAGE_LATENCY_DATA_SIZE_BYTES 0x800000


uint32_t register_message_latency_messages (const char *service_levels_arg,
                                            uint32_t service_levels[const MESSAGE_LATENCY_MAX_PARALLEL_PATHS]);

#endif /* SOURCE_IBV_MESSAGE_LATENCY_IBV_MESSAGE_LATENCY_MESSAGES_H_ */
