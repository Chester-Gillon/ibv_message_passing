/*
 * @file ibv_controller_worker_messages.c
 * @date 11 Feb 2018
 * @author Chester Gillon
 * @brief Defines messages used for a controller and workers example of multiple processes communicating
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"
#include "ibv_controller_worker_messages.h"

#define NUM_MESSAGE_BUFFERS 16

/**
 * @brief Register the definition of all communication paths between the controller and workers
 * @param[in] controller_and_workers_on_seperate_pcs
 *              Determines how the Infiniband ports are configured for the communication paths:
 *              a. When false the controller and workers are on the same PC with an external loopback from Infiniband
 *                 port 1 to 2.
 *              b. When true the controller is one one PC and the workers are on a different PC, and both ports are used.
 *                 There are is a connection from:
 *                 - Controller PC Infiniband port 1 to worker PC Infiniband port 1
 *                 - Controller PC Infiniband port 2 to worker PC Infiniband port 2
 */
void register_controller_worker_messages (const bool controller_and_workers_on_separate_pcs)
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
        if (controller_and_workers_on_separate_pcs)
        {
            path_def.source_ib_device = "mlx4_0";
            path_def.source_port_num = (worker_node_id % 2) + 1;
            path_def.destination_ib_device = "mlx4_0";
            path_def.destination_port_num = (worker_node_id % 2) + 1;
        }
        else
        {
            path_def.source_ib_device = "mlx4_0";
            path_def.source_port_num = 1;
            path_def.destination_ib_device = "mlx4_0";
            path_def.destination_port_num = 2;
        }
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
        if (controller_and_workers_on_separate_pcs)
        {
            path_def.source_ib_device = "mlx4_0";
            path_def.source_port_num = (worker_node_id % 2) + 1;
            path_def.destination_ib_device = "mlx4_0";
            path_def.destination_port_num = (worker_node_id % 2) + 1;
        }
        else
        {
            path_def.source_ib_device = "mlx4_0";
            path_def.source_port_num = 2;
            path_def.destination_ib_device = "mlx4_0";
            path_def.destination_port_num = 1;
        }
        path_def.max_message_size = sizeof (worker_to_controller_msgs);
        path_def.num_message_buffers = NUM_MESSAGE_BUFFERS;
        register_path_definition (&path_def);
    }
}
