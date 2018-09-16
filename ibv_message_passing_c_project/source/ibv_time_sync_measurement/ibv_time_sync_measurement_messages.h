/*
 * @file ibv_time_sync_measurement_messages.h
 * @date 16 Sep 2018
 * @author Chester Gillon
 * @brief Defines messages used to measure time synchronisation between two PCs.
 */

#ifndef SOURCE_IBV_TIME_SYNC_MEASUREMENT_IBV_TIME_SYNC_MEASUREMENT_MESSAGES_H_
#define SOURCE_IBV_TIME_SYNC_MEASUREMENT_IBV_TIME_SYNC_MEASUREMENT_MESSAGES_H_


/* The node identities for the master and slave */
typedef enum
{
    TIME_SYNC_MASTER_NODE_ID,
    TIME_SYNC_SLAVE_NODE_ID
} time_sync_measurement_node_ids;

/** The instances of different paths between the master and slave */
typedef enum
{
    TIME_SYNC_MASTER_TO_SLAVE_PATH,
    TIME_SYNC_SLAVE_TO_MASTER_PATH
} time_sync_measurement_path_instances;

/** The message IDs exchanged between the master and slave */
typedef enum
{
    /* Header only message sent from master to slave to indicate the master is ready for the test */
    TIME_SYNC_MASTER_READY_MSG_ID,
    /* Header only message sent from slave to master in response to TIME_SYNC_MASTER_READY_MSG_ID */
    TIME_SYNC_SLAVE_READY_MSG_ID,
    /* Header only message sent from master to slave to request the current slave CLOCK_REALTIME time */
    TIME_SYNC_TIME_REQUEST,
    /* Message sent from slave to master in response to TIME_SYNC_SLAVE_READY_MSG_ID, containing the current slave
     * CLOCK_REALTIME time */
    TIME_SYNC_TIME_REPLY,
    /* Header only message sent from master to slave to indicate the test is complete and the slave is to exit */
    TIME_SYNC_TEST_COMPLETE
} time_sync_measurment_msg_ids;

/* Message body for TIME_SYNC_TIME_REPLY */
typedef struct
{
    struct timespec slave_current_time;
} slave_current_time_msg;


void register_time_sync_measurement_messages (void);

#endif /* SOURCE_IBV_TIME_SYNC_MEASUREMENT_IBV_TIME_SYNC_MEASUREMENT_MESSAGES_H_ */
