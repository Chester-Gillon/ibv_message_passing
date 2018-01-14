-- @file ibv_ada_structure_size.adb
-- @date 14 Jan 2018
-- @author Chester Gillon
-- @brief Display the size and offsets of the structure fields in ibv_message_bw_interface_h
-- @details The format of the output from this program matches the C source file ibv_c_structure_size.c.
--          This is so that the output from the two programs can be compared to see if the Ada records
--          have the same memory layout as the C structures.

with ada.Text_IO;
with Ada.Strings.Fixed;
with ibv_message_bw_interface_h;

procedure Ibv_Ada_Structure_Size is

   procedure print_struct_size (struct_name: in string; struct_size: in Natural) is
   begin
      ada.Text_IO.Put_Line (struct_name & "," &
                              Ada.Strings.Fixed.Trim (struct_size'Image, Ada.Strings.Left));
   end print_struct_size;

   procedure print_struct_field (struct_name: in string; field_name: in string;
                                 field_size: in Natural; field_position: in Natural) is
   begin
      ada.Text_IO.Put_Line (struct_name & "." & field_name & "," &
                              ada.Strings.Fixed.Trim (field_size'Image, Ada.Strings.Left) & "," &
                              ada.Strings.Fixed.Trim (field_position'Image, Ada.Strings.Left));
   end print_struct_field;

   procedure report_communication_path_definition is
      struct_name : constant string := "communication_path_definition";
      path_def: ibv_message_bw_interface_h.communication_path_definition;
   begin
      print_struct_size (struct_name, path_def'Size);
      print_struct_field (struct_name, "instance", path_def.instance'Size, path_def.instance'Position);
      print_struct_field (struct_name, "ib_device", path_def.ib_device'Size, path_def.ib_device'Position);
      print_struct_field (struct_name, "port_num", path_def.port_num'Size, path_def.port_num'Position);
      print_struct_field (struct_name, "max_message_size", path_def.max_message_size'Size, path_def.max_message_size'Position);
      print_struct_field (struct_name, "num_message_buffers", path_def.num_message_buffers'Size, path_def.num_message_buffers'Position);
      print_struct_field (struct_name, "allocation_type", path_def.allocation_type'Size, path_def.allocation_type'Position);
      print_struct_field (struct_name, "tx_polls_for_errors", path_def.tx_polls_for_errors'Size, path_def.tx_polls_for_errors'Position);
      print_struct_field (struct_name, "tx_checks_memory_buffer_size", path_def.tx_checks_memory_buffer_size'Size, path_def.tx_checks_memory_buffer_size'Position);
   end report_communication_path_definition;

   procedure report_ib_port_endpoint is
      struct_name : constant string := "ib_port_endpoint";
      endpoint : ibv_message_bw_interface_h.ib_port_endpoint;
      offset : Natural;
   begin
      print_struct_size (struct_name, endpoint'Size);
      print_struct_field (struct_name, "num_devices", endpoint.num_devices'Size, endpoint.num_devices'Position);
      print_struct_field (struct_name, "device_list", endpoint.device_list'Size, endpoint.device_list'Position);
      print_struct_field (struct_name, "selected_device", endpoint.selected_device'Size, endpoint.selected_device'Position);
      print_struct_field (struct_name, "device_context", endpoint.device_context'Size, endpoint.device_context'Position);
      print_struct_field (struct_name, "device_pd", endpoint.device_pd'Size, endpoint.device_pd'Position);
      print_struct_field (struct_name, "device_attributes", endpoint.device_attributes'Size, endpoint.device_attributes'Position);
      offset := endpoint.device_attributes'Position;
      print_struct_field (struct_name, "device_attributes.fw_ver", endpoint.device_attributes.fw_ver'Size, offset + endpoint.device_attributes.fw_ver'Position);
      print_struct_field (struct_name, "device_attributes.node_guid", endpoint.device_attributes.node_guid'Size, offset + endpoint.device_attributes.node_guid'Position);
      print_struct_field (struct_name, "device_attributes.sys_image_guid", endpoint.device_attributes.sys_image_guid'Size, offset + endpoint.device_attributes.sys_image_guid'Position);
      print_struct_field (struct_name, "device_attributes.max_mr_size", endpoint.device_attributes.max_mr_size'Size, offset + endpoint.device_attributes.max_mr_size'Position);
      print_struct_field (struct_name, "device_attributes.page_size_cap", endpoint.device_attributes.page_size_cap'Size, offset + endpoint.device_attributes.page_size_cap'Position);
      print_struct_field (struct_name, "device_attributes.vendor_id", endpoint.device_attributes.vendor_id'Size, offset + endpoint.device_attributes.vendor_id'Position);
      print_struct_field (struct_name, "device_attributes.vendor_part_id", endpoint.device_attributes.vendor_part_id'Size, offset + endpoint.device_attributes.vendor_part_id'Position);
      print_struct_field (struct_name, "device_attributes.hw_ver", endpoint.device_attributes.hw_ver'Size, offset + endpoint.device_attributes.hw_ver'Position);
      print_struct_field (struct_name, "device_attributes.max_qp", endpoint.device_attributes.max_qp'Size, offset + endpoint.device_attributes.max_qp'Position);
      print_struct_field (struct_name, "device_attributes.max_qp_wr", endpoint.device_attributes.max_qp_wr'Size, offset + endpoint.device_attributes.max_qp_wr'Position);
      print_struct_field (struct_name, "device_attributes.device_cap_flags", endpoint.device_attributes.device_cap_flags'Size, offset + endpoint.device_attributes.device_cap_flags'Position);
      print_struct_field (struct_name, "device_attributes.max_sge", endpoint.device_attributes.max_sge'Size, offset + endpoint.device_attributes.max_sge'Position);
      print_struct_field (struct_name, "device_attributes.max_sge_rd", endpoint.device_attributes.max_sge_rd'Size, offset + endpoint.device_attributes.max_sge_rd'Position);
      print_struct_field (struct_name, "device_attributes.max_cq", endpoint.device_attributes.max_cq'Size, offset + endpoint.device_attributes.max_cq'Position);
      print_struct_field (struct_name, "device_attributes.max_cqe", endpoint.device_attributes.max_cqe'Size, offset + endpoint.device_attributes.max_cqe'Position);
      print_struct_field (struct_name, "device_attributes.max_mr", endpoint.device_attributes.max_mr'Size, offset + endpoint.device_attributes.max_mr'Position);
      print_struct_field (struct_name, "device_attributes.max_pd", endpoint.device_attributes.max_pd'Size, offset + endpoint.device_attributes.max_pd'Position);
      print_struct_field (struct_name, "device_attributes.max_qp_rd_atom", endpoint.device_attributes.max_qp_rd_atom'Size, offset + endpoint.device_attributes.max_qp_rd_atom'Position);
      print_struct_field (struct_name, "device_attributes.max_ee_rd_atom", endpoint.device_attributes.max_ee_rd_atom'Size, offset + endpoint.device_attributes.max_ee_rd_atom'Position);
      print_struct_field (struct_name, "device_attributes.max_res_rd_atom", endpoint.device_attributes.max_res_rd_atom'Size, offset + endpoint.device_attributes.max_res_rd_atom'Position);
      print_struct_field (struct_name, "device_attributes.max_qp_init_rd_atom", endpoint.device_attributes.max_qp_init_rd_atom'Size, offset + endpoint.device_attributes.max_qp_init_rd_atom'Position);
      print_struct_field (struct_name, "device_attributes.max_ee_init_rd_atom", endpoint.device_attributes.max_ee_init_rd_atom'Size, offset + endpoint.device_attributes.max_ee_init_rd_atom'Position);
      print_struct_field (struct_name, "device_attributes.atomic_cap", endpoint.device_attributes.atomic_cap'Size, offset + endpoint.device_attributes.atomic_cap'Position);
      print_struct_field (struct_name, "device_attributes.max_ee", endpoint.device_attributes.max_ee'Size, offset + endpoint.device_attributes.max_ee'Position);
      print_struct_field (struct_name, "device_attributes.max_rdd", endpoint.device_attributes.max_rdd'Size, offset + endpoint.device_attributes.max_rdd'Position);
      print_struct_field (struct_name, "device_attributes.max_mw", endpoint.device_attributes.max_mw'Size, offset + endpoint.device_attributes.max_mw'Position);
      print_struct_field (struct_name, "device_attributes.max_raw_ipv6_qp", endpoint.device_attributes.max_raw_ipv6_qp'Size, offset + endpoint.device_attributes.max_raw_ipv6_qp'Position);
      print_struct_field (struct_name, "device_attributes.max_raw_ethy_qp", endpoint.device_attributes.max_raw_ethy_qp'Size, offset + endpoint.device_attributes.max_raw_ethy_qp'Position);
      print_struct_field (struct_name, "device_attributes.max_mcast_grp", endpoint.device_attributes.max_mcast_grp'Size, offset + endpoint.device_attributes.max_mcast_grp'Position);
      print_struct_field (struct_name, "device_attributes.max_mcast_qp_attach", endpoint.device_attributes.max_mcast_qp_attach'Size, offset + endpoint.device_attributes.max_mcast_qp_attach'Position);
      print_struct_field (struct_name, "device_attributes.max_total_mcast_qp_attach", endpoint.device_attributes.max_total_mcast_qp_attach'Size, offset + endpoint.device_attributes.max_total_mcast_qp_attach'Position);
      print_struct_field (struct_name, "device_attributes.max_ah", endpoint.device_attributes.max_ah'Size, offset + endpoint.device_attributes.max_ah'Position);
      print_struct_field (struct_name, "device_attributes.max_fmr", endpoint.device_attributes.max_fmr'Size, offset + endpoint.device_attributes.max_fmr'Position);
      print_struct_field (struct_name, "device_attributes.max_map_per_fmr", endpoint.device_attributes.max_map_per_fmr'Size, offset + endpoint.device_attributes.max_map_per_fmr'Position);
      print_struct_field (struct_name, "device_attributes.max_srq", endpoint.device_attributes.max_srq'Size, offset + endpoint.device_attributes.max_srq'Position);
      print_struct_field (struct_name, "device_attributes.max_srq_wr", endpoint.device_attributes.max_srq_wr'Size, offset + endpoint.device_attributes.max_srq_wr'Position);
      print_struct_field (struct_name, "device_attributes.max_srq_sge", endpoint.device_attributes.max_srq_sge'Size, offset + endpoint.device_attributes.max_srq_sge'Position);
      print_struct_field (struct_name, "device_attributes.max_pkeys", endpoint.device_attributes.max_pkeys'Size, offset + endpoint.device_attributes.max_pkeys'Position);
      print_struct_field (struct_name, "device_attributes.local_ca_ack_delay", endpoint.device_attributes.local_ca_ack_delay'Size, offset + endpoint.device_attributes.local_ca_ack_delay'Position);
      print_struct_field (struct_name, "device_attributes.phys_port_cnt", endpoint.device_attributes.phys_port_cnt'Size, offset + endpoint.device_attributes.phys_port_cnt'Position);
      print_struct_field (struct_name, "port_attributes", endpoint.port_attributes'Size, endpoint.port_attributes'Position);
      offset := endpoint.port_attributes'Position;
      print_struct_field (struct_name, "port_attributes.state", endpoint.port_attributes.state'Size, offset + endpoint.port_attributes.state'Position);
      print_struct_field (struct_name, "port_attributes.max_mtu", endpoint.port_attributes.max_mtu'Size, offset + endpoint.port_attributes.max_mtu'Position);
      print_struct_field (struct_name, "port_attributes.active_mtu", endpoint.port_attributes.active_mtu'Size, offset + endpoint.port_attributes.active_mtu'Position);
      print_struct_field (struct_name, "port_attributes.gid_tbl_len", endpoint.port_attributes.gid_tbl_len'Size, offset + endpoint.port_attributes.gid_tbl_len'Position);
      print_struct_field (struct_name, "port_attributes.port_cap_flags", endpoint.port_attributes.port_cap_flags'Size, offset + endpoint.port_attributes.port_cap_flags'Position);
      print_struct_field (struct_name, "port_attributes.max_msg_sz", endpoint.port_attributes.max_msg_sz'Size, offset + endpoint.port_attributes.max_msg_sz'Position);
      print_struct_field (struct_name, "port_attributes.bad_pkey_cntr", endpoint.port_attributes.bad_pkey_cntr'Size, offset + endpoint.port_attributes.bad_pkey_cntr'Position);
      print_struct_field (struct_name, "port_attributes.qkey_viol_cntr", endpoint.port_attributes.qkey_viol_cntr'Size, offset + endpoint.port_attributes.qkey_viol_cntr'Position);
      print_struct_field (struct_name, "port_attributes.pkey_tbl_len", endpoint.port_attributes.pkey_tbl_len'Size, offset + endpoint.port_attributes.pkey_tbl_len'Position);
      print_struct_field (struct_name, "port_attributes.lid", endpoint.port_attributes.lid'Size, offset + endpoint.port_attributes.lid'Position);
      print_struct_field (struct_name, "port_attributes.sm_lid", endpoint.port_attributes.sm_lid'Size, offset + endpoint.port_attributes.sm_lid'Position);
      print_struct_field (struct_name, "port_attributes.lmc", endpoint.port_attributes.lmc'Size, offset + endpoint.port_attributes.lmc'Position);
      print_struct_field (struct_name, "port_attributes.max_vl_num", endpoint.port_attributes.max_vl_num'Size, offset + endpoint.port_attributes.max_vl_num'Position);
      print_struct_field (struct_name, "port_attributes.sm_sl", endpoint.port_attributes.sm_sl'Size, offset + endpoint.port_attributes.sm_sl'Position);
      print_struct_field (struct_name, "port_attributes.subnet_timeout", endpoint.port_attributes.subnet_timeout'Size, offset + endpoint.port_attributes.subnet_timeout'Position);
      print_struct_field (struct_name, "port_attributes.init_type_reply", endpoint.port_attributes.init_type_reply'Size, offset + endpoint.port_attributes.init_type_reply'Position);
      print_struct_field (struct_name, "port_attributes.active_width", endpoint.port_attributes.active_width'Size, offset + endpoint.port_attributes.active_width'Position);
      print_struct_field (struct_name, "port_attributes.active_speed", endpoint.port_attributes.active_speed'Size, offset + endpoint.port_attributes.active_speed'Position);
      print_struct_field (struct_name, "port_attributes.phys_state", endpoint.port_attributes.phys_state'Size, offset + endpoint.port_attributes.phys_state'Position);
      print_struct_field (struct_name, "port_attributes.link_layer", endpoint.port_attributes.link_layer'Size, offset + endpoint.port_attributes.link_layer'Position);
      print_struct_field (struct_name, "port_attributes.reserved", endpoint.port_attributes.reserved'Size, offset + endpoint.port_attributes.reserved'Position);
   end report_ib_port_endpoint;

   procedure report_memory_buffer_attributes is
      struct_name : constant string := "memory_buffer_attributes";
      buffer : ibv_message_bw_interface_h.memory_buffer_attributes;
   begin
      print_struct_size (struct_name, buffer'Size);
      print_struct_field (struct_name, "size", buffer.size'Size, buffer.size'Position);
      print_struct_field (struct_name, "addr", buffer.addr'Size, buffer.addr'Position);
      print_struct_field (struct_name, "rkey", buffer.rkey'Size, buffer.rkey'Position);
      print_struct_field (struct_name, "lid", buffer.lid'Size, buffer.lid'Position);
      print_struct_field (struct_name, "psn", buffer.psn'Size, buffer.psn'Position);
      print_struct_field (struct_name, "qp_num", buffer.qp_num'Size, buffer.qp_num'Position);
      print_struct_field (struct_name, "qp_ready_to_receive", buffer.qp_ready_to_receive'Size, buffer.qp_ready_to_receive'Position);
   end report_memory_buffer_attributes;

   procedure report_communication_path_slp_connection is
      struct_name : constant string := "communication_path_slp_connection";
      slp_connection : ibv_message_bw_interface_h.communication_path_slp_connection;
      offset : Natural;
   begin
      print_struct_size (struct_name, slp_connection'Size);
      print_struct_field (struct_name, "handle", slp_connection.handle'Size, slp_connection.handle'Position);
      print_struct_field (struct_name, "local_service_url", slp_connection.local_service_url'Size, slp_connection.local_service_url'Position);
      print_struct_field (struct_name, "remote_service_name", slp_connection.remote_service_name'Size, slp_connection.remote_service_name'Position);
      print_struct_field (struct_name, "remote_service_url", slp_connection.remote_service_url'Size, slp_connection.remote_service_url'Position);
      print_struct_field (struct_name, "remote_attributes_obtained", slp_connection.remote_attributes_obtained'Size, slp_connection.remote_attributes_obtained'Position);
      print_struct_field (struct_name, "local_attributes", slp_connection.local_attributes'Size, slp_connection.local_attributes'Position);
      offset := slp_connection.local_attributes'Position;
      print_struct_field (struct_name, "local_attributes.size", slp_connection.local_attributes.size'Size, offset + slp_connection.local_attributes.size'Position);
      print_struct_field (struct_name, "local_attributes.addr", slp_connection.local_attributes.addr'Size, offset + slp_connection.local_attributes.addr'Position);
      print_struct_field (struct_name, "local_attributes.rkey", slp_connection.local_attributes.rkey'Size, offset + slp_connection.local_attributes.rkey'Position);
      print_struct_field (struct_name, "local_attributes.lid", slp_connection.local_attributes.lid'Size, offset + slp_connection.local_attributes.lid'Position);
      print_struct_field (struct_name, "local_attributes.psn", slp_connection.local_attributes.psn'Size, offset + slp_connection.local_attributes.psn'Position);
      print_struct_field (struct_name, "local_attributes.qp_num", slp_connection.local_attributes.qp_num'Size, offset + slp_connection.local_attributes.qp_num'Position);
      print_struct_field (struct_name, "local_attributes.qp_ready_to_receive", slp_connection.local_attributes.qp_ready_to_receive'Size, offset + slp_connection.local_attributes.qp_ready_to_receive'Position);
      print_struct_field (struct_name, "remote_attributes", slp_connection.remote_attributes'Size, slp_connection.remote_attributes'Position);
      offset := slp_connection.remote_attributes'Position;
      print_struct_field (struct_name, "remote_attributes.size", slp_connection.remote_attributes.size'Size, offset + slp_connection.remote_attributes.size'Position);
      print_struct_field (struct_name, "remote_attributes.addr", slp_connection.remote_attributes.addr'Size, offset + slp_connection.remote_attributes.addr'Position);
      print_struct_field (struct_name, "remote_attributes.rkey", slp_connection.remote_attributes.rkey'Size, offset + slp_connection.remote_attributes.rkey'Position);
      print_struct_field (struct_name, "remote_attributes.lid", slp_connection.remote_attributes.lid'Size, offset + slp_connection.remote_attributes.lid'Position);
      print_struct_field (struct_name, "remote_attributes.psn", slp_connection.remote_attributes.psn'Size, offset + slp_connection.remote_attributes.psn'Position);
      print_struct_field (struct_name, "remote_attributes.qp_num", slp_connection.remote_attributes.qp_num'Size, offset + slp_connection.remote_attributes.qp_num'Position);
      print_struct_field (struct_name, "remote_attributes.qp_ready_to_receive", slp_connection.remote_attributes.qp_ready_to_receive'Size, offset + slp_connection.remote_attributes.qp_ready_to_receive'Position);
   end report_communication_path_slp_connection;

   procedure report_memory_buffer is
      struct_name : constant string := "memory_buffer";
      buffer : ibv_message_bw_interface_h.memory_buffer;
   begin
      print_struct_size (struct_name, buffer'Size);
      print_struct_field (struct_name, "allocation_type", buffer.allocation_type'Size, buffer.allocation_type'Position);
      print_struct_field (struct_name, "size", buffer.size'Size, buffer.size'Position);
      print_struct_field (struct_name, "buffer", buffer.buffer'Size, buffer.buffer'Position);
      print_struct_field (struct_name, "pathname", buffer.pathname'Size, buffer.pathname'Position);
      print_struct_field (struct_name, "fd", buffer.fd'Size, buffer.fd'Position);
   end report_memory_buffer;

   procedure report_message_header is
      struct_name : constant string := "message_header";
      header : ibv_message_bw_interface_h.message_header;
   begin
      print_struct_size (struct_name, header'Size);
      print_struct_field (struct_name, "sequence_number", header.sequence_number'Size, header.sequence_number'Position);
      print_struct_field (struct_name, "message_length", header.message_length'Size, header.message_length'Position);
      print_struct_field (struct_name, "message_id", header.message_id'Size, header.message_id'Position);
      print_struct_field (struct_name, "source_instance", header.source_instance'Size, header.source_instance'Position);
   end report_message_header;

   procedure report_api_message_buffer is
      struct_name : constant string := "api_message_buffer";
      buffer : ibv_message_bw_interface_h.api_message_buffer;
   begin
      print_struct_size (struct_name, buffer'Size);
      print_struct_field (struct_name, "header", buffer.header'Size, buffer.header'Position);
      print_struct_field (struct_name, "data", buffer.data'Size, buffer.data'Position);
      print_struct_field (struct_name, "buffer_index", buffer.buffer_index'Size, buffer.buffer_index'Position);
   end report_api_message_buffer;

begin
   ada.Text_IO.Put_Line ("Structure name,Size (bits),Offset (bytes)");
   report_communication_path_definition;
   report_ib_port_endpoint;
   report_memory_buffer_attributes;
   report_communication_path_slp_connection;
   report_memory_buffer;
   report_message_header;
   report_api_message_buffer;
end Ibv_Ada_Structure_Size;

