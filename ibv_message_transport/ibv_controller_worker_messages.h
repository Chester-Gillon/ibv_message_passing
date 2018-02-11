/*
 * @file ibv_controller_worker_messages.h
 * @date 11 Feb 2018
 * @author Chester Gillon
 * @brief Defines messages used for a controller and workers example of multiple processes communicating
 */

#ifndef IBV_CONTROLLER_WORKER_MESSAGES_H_
#define IBV_CONTROLLER_WORKER_MESSAGES_H_

/** Number of worker processes connected to the controller process */
#define NUM_WORKERS 3

/** The node identities of the controller and workers */
typedef enum
{
    CONTROLLER_NODE_ID,
    FIRST_WORKER_NODE_ID,
    LAST_WORKER_NODE_ID = (FIRST_WORKER_NODE_ID + NUM_WORKERS - 1)
} controller_worker_node_ids;

/** The instances of different paths between the controller and workers */
typedef enum
{
    CONTROLLER_TO_WORKER_PATH,
    WORKER_TO_CONTROLLER_PATH
} controller_worker_path_instances;

typedef enum
{
    /** Initial message sent from a worker indicating it is ready to start the test */
    CW_WORKER_READY,
    /** Message sent to the workers to indicate the test is complete, and the worker should shutdown */
    CW_REQUEST_SHUTDOWN
} controller_worker_msg_ids;

/** Contents of the CW_WORKER_READY message sent from a worker to the controller */
typedef struct
{
    /** The number of valid characters in worker_executable_pathname */
    uint32_t worker_executable_pathname_len;
    /** The absolute pathname of the worker executable */
    char worker_executable_pathname[PATH_MAX];
} worker_ready_msg;

/** Contents of the CW_REQUEST_SHUTDOWN message sent from the controller to each worker */
typedef struct
{
    /** The total number of work requests which have been processed by each worker */
    uint32_t num_requests_per_worker[NUM_WORKERS];
} request_shutdown_msg;

/** The different message structures which can be sent from a worker to the controller, for sizing communication paths */
typedef union
{
    worker_ready_msg worker_ready;
} worker_to_controller_msgs;

/** The different message structures which can be sent from the controller to each worker, for sizing communication paths */
typedef union
{
    request_shutdown_msg request_shutdown;
} controller_to_worker_msgs;

void register_controller_worker_messages (void);

#endif /* IBV_CONTROLLER_WORKER_MESSAGES_H_ */
