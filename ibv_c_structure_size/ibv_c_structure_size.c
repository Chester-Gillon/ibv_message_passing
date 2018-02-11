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
#include <stdio.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

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
    PRINT_STRUCT_FIELD (communication_path_definition, max_message_size);
    PRINT_STRUCT_FIELD (communication_path_definition, num_message_buffers);
    PRINT_STRUCT_FIELD (communication_path_definition, allocation_type);
    PRINT_STRUCT_FIELD (communication_path_definition, tx_polls_for_errors);
    PRINT_STRUCT_FIELD (communication_path_definition, tx_checks_memory_buffer_size);

    PRINT_STRUCT_SIZE (ib_port_endpoint);
    PRINT_STRUCT_FIELD (ib_port_endpoint, num_devices);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_list);
    PRINT_STRUCT_FIELD (ib_port_endpoint, selected_device);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_context);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_pd);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.fw_ver);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.node_guid);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.sys_image_guid);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_mr_size);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.page_size_cap);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.vendor_id);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.vendor_part_id);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.hw_ver);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_qp);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_qp_wr);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.device_cap_flags);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_sge);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_sge_rd);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_cq);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_cqe);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_mr);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_pd);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_qp_rd_atom);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_ee_rd_atom);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_res_rd_atom);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_qp_init_rd_atom);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_ee_init_rd_atom);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.atomic_cap);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_ee);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_rdd);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_mw);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_raw_ipv6_qp);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_raw_ethy_qp);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_mcast_grp);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_mcast_qp_attach);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_total_mcast_qp_attach);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_ah);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_fmr);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_map_per_fmr);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_srq);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_srq_wr);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_srq_sge);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.max_pkeys);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.local_ca_ack_delay);
    PRINT_STRUCT_FIELD (ib_port_endpoint, device_attributes.phys_port_cnt);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.state);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.max_mtu);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.active_mtu);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.gid_tbl_len);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.port_cap_flags);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.max_msg_sz);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.bad_pkey_cntr);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.qkey_viol_cntr);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.pkey_tbl_len);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.lid);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.sm_lid);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.lmc);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.max_vl_num);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.sm_sl);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.subnet_timeout);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.init_type_reply);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.active_width);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.active_speed);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.phys_state);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.link_layer);
    PRINT_STRUCT_FIELD (ib_port_endpoint, port_attributes.reserved);

    PRINT_STRUCT_SIZE (memory_buffer_attributes);
    PRINT_STRUCT_FIELD (memory_buffer_attributes, size);
    PRINT_STRUCT_FIELD (memory_buffer_attributes, addr);
    PRINT_STRUCT_FIELD (memory_buffer_attributes, rkey);
    PRINT_STRUCT_FIELD (memory_buffer_attributes, lid);
    PRINT_STRUCT_FIELD (memory_buffer_attributes, psn);
    PRINT_STRUCT_FIELD (memory_buffer_attributes, qp_num);
    PRINT_STRUCT_FIELD (memory_buffer_attributes, qp_ready_to_receive);

    PRINT_STRUCT_SIZE (communication_path_slp_connection);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, handle);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_service_url);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_service_name);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_service_url);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes_obtained);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes.size);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes.addr);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes.rkey);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes.lid);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes.psn);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes.qp_num);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, local_attributes.qp_ready_to_receive);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes.size);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes.addr);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes.rkey);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes.lid);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes.psn);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes.qp_num);
    PRINT_STRUCT_FIELD (communication_path_slp_connection, remote_attributes.qp_ready_to_receive);

    PRINT_STRUCT_SIZE (memory_buffer);
    PRINT_STRUCT_FIELD (memory_buffer, allocation_type);
    PRINT_STRUCT_FIELD (memory_buffer, size);
    PRINT_STRUCT_FIELD (memory_buffer, buffer);
    PRINT_STRUCT_FIELD (memory_buffer, pathname);
    PRINT_STRUCT_FIELD (memory_buffer, fd);

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
