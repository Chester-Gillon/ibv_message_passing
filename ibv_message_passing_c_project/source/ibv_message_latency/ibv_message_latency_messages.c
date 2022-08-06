/*
 * @file ibv_message_latency_messages.c
 * @date 18 Nov 2018
 * @author Chester Gillon
 * @brief Contains functions used by the message latency master and slave
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#include "ibv_message_bw_interface.h"
#include "ibv_message_latency_messages.h"


/**
 * @brief Register the communication paths used for measuring latency.
 * @details Only a single buffer is used for each path to synchronise the master and slave on each test iteration.
 * @param[in] service_levels_arg A comma separated list of the Infiniband service levels to be used on the communication
 *            paths for measuring latency.
 * @param[out] service_levels The service level values extracted from service_levels_in
 * @return Returns the number of data paths which have been registered, according to number of values in service_levels_in.
 */
uint32_t register_message_latency_messages (const char *service_levels_arg,
                                            uint32_t service_levels[const MESSAGE_LATENCY_MAX_PARALLEL_PATHS])
{
    char *const service_levels_text = strdup (service_levels_arg);
    const char *const delimiters = ",";
    char *token;
    char *saveptr;
    communication_path_definition path_def;
    uint32_t num_data_paths;
    uint32_t ib_sl;
    char junk;

    memset (&path_def, 0, sizeof (path_def));
    path_def.allocation_type = BUFFER_ALLOCATION_SHARED_MEMORY;
    path_def.tx_checks_memory_buffer_size = true;
    path_def.tx_polls_for_errors = false;
    path_def.set_non_default_retry_timeout = false;

    /* Register each of the parallel paths for measuring latency, where the service levels are extracted from the command line
     * argument. */
    num_data_paths = 0;
    token = strtok_r (service_levels_text, delimiters, &saveptr);
    while (token != NULL)
    {
        if ((sscanf (token, "%u%c", &ib_sl, &junk) != 1) || (ib_sl > 15))
        {
            fprintf (stderr, "Invalid service level %s\n", token);
            exit (EXIT_FAILURE);
        }
        if (num_data_paths == MESSAGE_LATENCY_MAX_PARALLEL_PATHS)
        {
            fprintf (stderr, "Too many service levels\n");
            exit (EXIT_FAILURE);
        }
        service_levels[num_data_paths] = ib_sl;

        path_def.source_node = MESSAGE_LATENCY_MASTER_NODE_ID;
        path_def.destination_node = MESSAGE_LATENCY_SLAVE_NODE_ID;
        path_def.instance = MASTER_TEST_MESSAGES_FIRST_PATH + num_data_paths;
        path_def.source_ib_device = "mlx4_0";
        path_def.source_port_num = 1;
        path_def.source_gid_index = 0;
        path_def.destination_ib_device = "mlx4_0";
        path_def.destination_port_num = 1;
        path_def.destination_gid_index = 0;
        path_def.service_level = ib_sl;
        path_def.max_message_size = MESSAGE_LATENCY_DATA_SIZE_BYTES;
        path_def.num_message_buffers = 1;
        register_path_definition (&path_def);

        num_data_paths++;
        token = strtok_r (NULL, delimiters, &saveptr);
    }
    free (service_levels_text);

    /* Register the path from the slave to master */
    path_def.source_node = MESSAGE_LATENCY_SLAVE_NODE_ID;
    path_def.destination_node = MESSAGE_LATENCY_MASTER_NODE_ID;
    path_def.instance = SLAVE_ACK_PATH;
    path_def.source_ib_device = "mlx4_0";
    path_def.source_port_num = 2;
    path_def.source_gid_index = 0;
    path_def.destination_ib_device = "mlx4_0";
    path_def.destination_port_num = 2;
    path_def.destination_gid_index = 0;
    path_def.service_level = DEFAULT_SERVICE_LEVEL;
    path_def.max_message_size = sizeof (latency_ack_msg);
    path_def.num_message_buffers = 1;
    register_path_definition (&path_def);

    return num_data_paths;
}
