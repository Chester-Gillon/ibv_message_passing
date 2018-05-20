/*
 * @file ibv_controller_worker_messages.c
 * @date 11 Feb 2018
 * @author Chester Gillon
 * @brief Defines messages used for a controller and workers example of multiple processes communicating
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"
#include "ibv_controller_worker_messages.h"

#define NUM_MESSAGE_BUFFERS 16

/**
 * @brief Register the definition of all communication paths between the controller and workers
 */
void register_controller_worker_messages (void)
{
    communication_path_definition path_def;
    uint32_t worker_node_id;

    memset (&path_def, 0, sizeof (path_def));
    path_def.allocation_type = BUFFER_ALLOCATION_SHARED_MEMORY;
    path_def.tx_checks_memory_buffer_size = true;
    path_def.tx_polls_for_errors = false;

    /* Register the path from the controller to each worker */
    for (worker_node_id = FIRST_WORKER_NODE_ID; worker_node_id <= LAST_WORKER_NODE_ID; worker_node_id++)
    {
        path_def.source_node = CONTROLLER_NODE_ID;
        path_def.destination_node = worker_node_id;
        path_def.instance = CONTROLLER_TO_WORKER_PATH;
        path_def.source_ib_device = "mlx4_0";
        path_def.source_port_num = 1;
        path_def.destination_ib_device = "mlx4_0";
        path_def.destination_port_num = 2;
        path_def.max_message_size = sizeof (controller_to_worker_msgs);
        path_def.num_message_buffers = NUM_MESSAGE_BUFFERS;
        register_path_definition (&path_def);
    }

    /* Register the path from each worker to the controller */
    for (worker_node_id = FIRST_WORKER_NODE_ID; worker_node_id <= LAST_WORKER_NODE_ID; worker_node_id++)
    {
        path_def.source_node = worker_node_id;
        path_def.destination_node = CONTROLLER_NODE_ID;
        path_def.instance = WORKER_TO_CONTROLLER_PATH;
        path_def.source_ib_device = "mlx4_0";
        path_def.source_port_num = 2;
        path_def.destination_ib_device = "mlx4_0";
        path_def.destination_port_num = 1;
        path_def.max_message_size = sizeof (worker_to_controller_msgs);
        path_def.num_message_buffers = NUM_MESSAGE_BUFFERS;
        register_path_definition (&path_def);
    }
}
