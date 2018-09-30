/*
 * @file ibv_time_sync_measurement_messages.c
 * @date16 Sep 2018
 * @author Chester Gillon
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"
#include "ibv_time_sync_measurement_messages.h"


/**
 * @brief Register the definition of the messages passed between the time sync measurement master and slave.
 */
void register_time_sync_measurement_messages (void)
{
    communication_path_definition path_def;

    memset (&path_def, 0, sizeof (path_def));
    path_def.allocation_type = BUFFER_ALLOCATION_SHARED_MEMORY;
    path_def.tx_checks_memory_buffer_size = true;
    path_def.tx_polls_for_errors = false;

    /* Register the path from the master to slave, which only has a single outstanding message at once */
    path_def.source_node = TIME_SYNC_MASTER_NODE_ID;
    path_def.destination_node = TIME_SYNC_SLAVE_NODE_ID;
    path_def.instance = TIME_SYNC_MASTER_TO_SLAVE_PATH;
    path_def.source_ib_device = "mlx4_0";
    path_def.source_port_num = 1;
    path_def.destination_ib_device = "mlx4_0";
    path_def.destination_port_num = 1;
    path_def.max_message_size = 0;
    path_def.num_message_buffers = 1;
    register_path_definition (&path_def);

    /* Register the path from the slave to master, which only has a single outstanding message at once */
    path_def.source_node = TIME_SYNC_SLAVE_NODE_ID;
    path_def.destination_node = TIME_SYNC_MASTER_NODE_ID;
    path_def.instance = TIME_SYNC_SLAVE_TO_MASTER_PATH;
    path_def.source_ib_device = "mlx4_0";
    path_def.source_port_num = 2;
    path_def.destination_ib_device = "mlx4_0";
    path_def.destination_port_num = 2;
    path_def.max_message_size = sizeof (slave_current_time_msg);
    path_def.num_message_buffers = 1;
    register_path_definition (&path_def);
}
