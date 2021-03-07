/*
 * @file ibv_c_structure_size.c
 * @date 14 Jan 2018
 * @author Chester Gillon
 * @brief Display the size and offsets of the structure fields in ibv_message_bw_interface.h
 * @details The sizes are reported in bits, for comparing against the 'Size of the C structures converted
 *          to Ada records by the GNAT g++ -dump-ada-spec option.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <limits.h>

#include "ibv_message_bw_interface.h"
#include "ibv_controller_worker_messages.h"

#define PRINT_STRUCT_SIZE(struct_type) \
    printf (#struct_type ",%lu\n", sizeof (struct_type) * 8)

#define PRINT_STRUCT_FIELD(struct_type,field) \
    printf (#struct_type "." #field ",%lu,%lu\n", sizeof(((struct_type *)0)->field) * 8, offsetof(struct_type,field));

int main (int argc, char *argv[])
{
    printf ("Structure name,Size (bits),Offset (bytes)\n");
    PRINT_STRUCT_SIZE (communication_path_definition);
    PRINT_STRUCT_FIELD (communication_path_definition, source_node);
    PRINT_STRUCT_FIELD (communication_path_definition, destination_node);
    PRINT_STRUCT_FIELD (communication_path_definition, instance);
    PRINT_STRUCT_FIELD (communication_path_definition, source_ib_device);
    PRINT_STRUCT_FIELD (communication_path_definition, destination_ib_device);
    PRINT_STRUCT_FIELD (communication_path_definition, source_port_num);
    PRINT_STRUCT_FIELD (communication_path_definition, destination_port_num);
    PRINT_STRUCT_FIELD (communication_path_definition, service_level);
    PRINT_STRUCT_FIELD (communication_path_definition, max_message_size);
    PRINT_STRUCT_FIELD (communication_path_definition, num_message_buffers);
    PRINT_STRUCT_FIELD (communication_path_definition, allocation_type);
    PRINT_STRUCT_FIELD (communication_path_definition, tx_polls_for_errors);
    PRINT_STRUCT_FIELD (communication_path_definition, tx_checks_memory_buffer_size);
    PRINT_STRUCT_FIELD (communication_path_definition, set_non_default_retry_timeout);
    PRINT_STRUCT_FIELD (communication_path_definition, retry_timeout);

    PRINT_STRUCT_SIZE (message_header);
    PRINT_STRUCT_FIELD (message_header, sequence_number);
    PRINT_STRUCT_FIELD (message_header, message_length);
    PRINT_STRUCT_FIELD (message_header, message_id);
    PRINT_STRUCT_FIELD (message_header, source_instance);

    PRINT_STRUCT_SIZE (tx_api_message_buffer);
    PRINT_STRUCT_FIELD (tx_api_message_buffer, header);
    PRINT_STRUCT_FIELD (tx_api_message_buffer, data);
    PRINT_STRUCT_FIELD (tx_api_message_buffer, context);
    PRINT_STRUCT_FIELD (tx_api_message_buffer, buffer_index);

    PRINT_STRUCT_SIZE (rx_api_message_buffer);
    PRINT_STRUCT_FIELD (rx_api_message_buffer, header);
    PRINT_STRUCT_FIELD (rx_api_message_buffer, data);
    PRINT_STRUCT_FIELD (rx_api_message_buffer, context);
    PRINT_STRUCT_FIELD (rx_api_message_buffer, buffer_index);

    PRINT_STRUCT_SIZE (worker_ready_msg);
    PRINT_STRUCT_FIELD (worker_ready_msg, worker_executable_pathname_len);
    PRINT_STRUCT_FIELD (worker_ready_msg, worker_executable_pathname);

    PRINT_STRUCT_SIZE (request_shutdown_msg);
    PRINT_STRUCT_FIELD (request_shutdown_msg, num_requests_per_worker);

    PRINT_STRUCT_SIZE (sum_integers_msg);
    PRINT_STRUCT_FIELD (sum_integers_msg, request_id);
    PRINT_STRUCT_FIELD (sum_integers_msg, num_integers_to_sum);
    PRINT_STRUCT_FIELD (sum_integers_msg, integers_to_sum);

    PRINT_STRUCT_SIZE (sum_result_msg);
    PRINT_STRUCT_FIELD (sum_result_msg, request_id);
    PRINT_STRUCT_FIELD (sum_result_msg, sum);

    PRINT_STRUCT_SIZE (worker_to_controller_msgs);
    PRINT_STRUCT_SIZE (controller_to_worker_msgs);

    return 0;
}
