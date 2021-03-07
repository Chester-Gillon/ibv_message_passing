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
with ibv_controller_worker_messages_h;

-- Disable this warning since the functions declare variables only to use 'Size and 'Position attributes but not actually
-- read the contents.
pragma Warnings (Off, "variable ""*"" is read but never assigned");

procedure Ibv_Ada_Structure_Size is

   procedure print_struct_size (struct_name: in string; struct_size: in Natural) is
   begin
      ada.Text_IO.Put_Line (struct_name & "," &
                              Ada.Strings.Fixed.Trim (Natural'Image (struct_size), Ada.Strings.Left));
   end print_struct_size;

   procedure print_struct_field (struct_name: in string; field_name: in string;
                                 field_size: in Natural; field_position: in Natural) is
   begin
      ada.Text_IO.Put_Line (struct_name & "." & field_name & "," &
                              ada.Strings.Fixed.Trim (Natural'Image (field_size), Ada.Strings.Left) & "," &
                              ada.Strings.Fixed.Trim (Natural'Image (field_position), Ada.Strings.Left));
   end print_struct_field;

   procedure report_communication_path_definition is
      struct_name : constant string := "communication_path_definition";
      path_def: ibv_message_bw_interface_h.communication_path_definition;
   begin
      print_struct_size (struct_name, path_def'Size);
      print_struct_field (struct_name, "source_node", path_def.source_node'Size, path_def.source_node'Position);
      print_struct_field (struct_name, "destination_node", path_def.destination_node'Size, path_def.destination_node'Position);
      print_struct_field (struct_name, "instance", path_def.instance'Size, path_def.instance'Position);
      print_struct_field (struct_name, "source_ib_device", path_def.source_ib_device'Size, path_def.source_ib_device'Position);
      print_struct_field (struct_name, "destination_ib_device", path_def.destination_ib_device'Size, path_def.destination_ib_device'Position);
      print_struct_field (struct_name, "source_port_num", path_def.source_port_num'Size, path_def.source_port_num'Position);
      print_struct_field (struct_name, "destination_port_num", path_def.destination_port_num'Size, path_def.destination_port_num'Position);
      print_struct_field (struct_name, "service_level", path_def.service_level'Size, path_def.service_level'Position);
      print_struct_field (struct_name, "max_message_size", path_def.max_message_size'Size, path_def.max_message_size'Position);
      print_struct_field (struct_name, "num_message_buffers", path_def.num_message_buffers'Size, path_def.num_message_buffers'Position);
      print_struct_field (struct_name, "allocation_type", path_def.allocation_type'Size, path_def.allocation_type'Position);
      print_struct_field (struct_name, "tx_polls_for_errors", path_def.tx_polls_for_errors'Size, path_def.tx_polls_for_errors'Position);
      print_struct_field (struct_name, "tx_checks_memory_buffer_size", path_def.tx_checks_memory_buffer_size'Size, path_def.tx_checks_memory_buffer_size'Position);
      print_struct_field (struct_name, "set_non_default_retry_timeout", path_def.set_non_default_retry_timeout'Size, path_def.set_non_default_retry_timeout'Position);
      print_struct_field (struct_name, "retry_timeout", path_def.retry_timeout'Size, path_def.retry_timeout'Position);
   end report_communication_path_definition;

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

   procedure report_tx_api_message_buffer is
      struct_name : constant string := "tx_api_message_buffer";
      buffer : ibv_message_bw_interface_h.tx_api_message_buffer;
   begin
      print_struct_size (struct_name, buffer'Size);
      print_struct_field (struct_name, "header", buffer.header'Size, buffer.header'Position);
      print_struct_field (struct_name, "data", buffer.data'Size, buffer.data'Position);
      print_struct_field (struct_name, "context", buffer.context'Size, buffer.context'Position);
      print_struct_field (struct_name, "buffer_index", buffer.buffer_index'Size, buffer.buffer_index'Position);
   end report_tx_api_message_buffer;

   procedure report_rx_api_message_buffer is
      struct_name : constant string := "rx_api_message_buffer";
      buffer : ibv_message_bw_interface_h.rx_api_message_buffer;
   begin
      print_struct_size (struct_name, buffer'Size);
      print_struct_field (struct_name, "header", buffer.header'Size, buffer.header'Position);
      print_struct_field (struct_name, "data", buffer.data'Size, buffer.data'Position);
      print_struct_field (struct_name, "context", buffer.context'Size, buffer.context'Position);
      print_struct_field (struct_name, "buffer_index", buffer.buffer_index'Size, buffer.buffer_index'Position);
   end report_rx_api_message_buffer;

   procedure report_worker_ready_msg is
      struct_name : constant string := "worker_ready_msg";
      worker_ready : ibv_controller_worker_messages_h.worker_ready_msg;
   begin
      print_struct_size (struct_name, worker_ready'Size);
      print_struct_field (struct_name, "worker_executable_pathname_len", worker_ready.worker_executable_pathname_len'Size, worker_ready.worker_executable_pathname_len'Position);
      print_struct_field (struct_name, "worker_executable_pathname", worker_ready.worker_executable_pathname'Size, worker_ready.worker_executable_pathname'Position);
   end report_worker_ready_msg;

   procedure report_request_shutdown_msg is
      struct_name : constant string := "request_shutdown_msg";
      request_shutdown : ibv_controller_worker_messages_h.request_shutdown_msg;
   begin
      print_struct_size (struct_name, request_shutdown'Size);
      print_struct_field (struct_name, "num_requests_per_worker", request_shutdown.num_requests_per_worker'Size, request_shutdown.num_requests_per_worker'Position);
   end report_request_shutdown_msg;

   procedure report_sum_integers_msg is
      struct_name : constant string := "sum_integers_msg";
      sum_integers : ibv_controller_worker_messages_h.sum_integers_msg;
   begin
      print_struct_size (struct_name, sum_integers'Size);
      print_struct_field (struct_name, "request_id", sum_integers.request_id'Size, sum_integers.request_id'Position);
      print_struct_field (struct_name, "num_integers_to_sum", sum_integers.num_integers_to_sum'Size, sum_integers.num_integers_to_sum'Position);
      print_struct_field (struct_name, "integers_to_sum", sum_integers.integers_to_sum'Size, sum_integers.integers_to_sum'Position);
   end report_sum_integers_msg;

   procedure report_sum_result_msg is
      struct_name : constant string := "sum_result_msg";
      sum_results : ibv_controller_worker_messages_h.sum_result_msg;
   begin
      print_struct_size (struct_name, sum_results'Size);
      print_struct_field (struct_name, "request_id", sum_results.request_id'Size, sum_results.request_id'Position);
      print_struct_field (struct_name, "sum", sum_results.sum'Size, sum_results.sum'Position);
   end report_sum_result_msg;

   procedure report_worker_to_controller_msgs is
      struct_name : constant string := "worker_to_controller_msgs";
      msgs : ibv_controller_worker_messages_h.worker_to_controller_msgs;
   begin
      print_struct_size (struct_name, msgs'Size);
   end report_worker_to_controller_msgs;

   procedure report_controller_to_worker_msgs is
      struct_name : constant string := "controller_to_worker_msgs";
      msgs : ibv_controller_worker_messages_h.controller_to_worker_msgs;
   begin
      print_struct_size (struct_name, msgs'Size);
   end report_controller_to_worker_msgs;

begin
   ada.Text_IO.Put_Line ("Structure name,Size (bits),Offset (bytes)");
   report_communication_path_definition;
   report_message_header;
   report_tx_api_message_buffer;
   report_rx_api_message_buffer;
   report_worker_ready_msg;
   report_request_shutdown_msg;
   report_sum_integers_msg;
   report_sum_result_msg;
   report_worker_to_controller_msgs;
   report_controller_to_worker_msgs;
end Ibv_Ada_Structure_Size;

