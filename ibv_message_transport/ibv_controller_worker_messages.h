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
    CW_REQUEST_SHUTDOWN,
    /** Message sent to a worker to request the sum of a number of psuedo-random values */
    CW_SUM_INTEGERS,
    /** Message sent from a worker in response to a CW_SUM_INTEGERS message */
    CW_SUM_RESULT
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

/** Contents of the CW_SUM_INTEGERS message sent from the controller to a worker */
#define MAX_INTEGERS_TO_SUM 2048
typedef struct
{
    /** Used to identify the work request, to be returned in the CW_SUM_RESULT message */
    uint32_t request_id;
    /** How many values in the integers_to_sum array[] to sum */
    uint32_t num_integers_to_sum;
    /** Variable length array of integers for the worker to sum */
    uint32_t integers_to_sum[MAX_INTEGERS_TO_SUM];
} sum_integers_msg;

/** Contents of the CW_SUM_RESULT message sent from a worker to the controller */
typedef struct
{
    /** From the CW_SUM_INTEGERS message for which this is the reply */
    uint32_t request_id;
    /** The sum of the values in the CW_SUM_INTEGERS message */
    uint32_t sum;
} sum_result_msg;

/** The different message structures which can be sent from a worker to the controller, for sizing communication paths */
typedef union
{
    worker_ready_msg worker_ready;
    sum_result_msg sum_result;
} worker_to_controller_msgs;

/** The different message structures which can be sent from the controller to each worker, for sizing communication paths */
typedef union
{
    request_shutdown_msg request_shutdown;
    sum_integers_msg sum_integers;
} controller_to_worker_msgs;

void register_controller_worker_messages (void);

#endif /* IBV_CONTROLLER_WORKER_MESSAGES_H_ */
