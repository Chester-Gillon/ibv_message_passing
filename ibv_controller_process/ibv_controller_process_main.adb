-- @file ibv_controller_process_main.adb
-- @date 11 Feb 2018
-- @author Chester Gillon
-- @details The main for the controller process written in Ada with communicates with
--          worker processing written in C via the ibv_message_transport library.

with ada.Text_IO;
with Interfaces.C;
use Interfaces.C;
with ibv_message_bw_interface_h;
with ibv_controller_worker_messages_h;
with Ada.Assertions;

procedure Ibv_Controller_Process_Main is

   type all_workers is range ibv_controller_worker_messages_h.FIRST_WORKER_NODE_ID .. ibv_controller_worker_messages_h.LAST_WORKER_NODE_ID;
   type paths_to_workers_array is array (all_workers) of
      ibv_message_bw_interface_h.tx_message_context_handle;

   -- @brief Wait for all worker processes to report they are ready to run the test
   procedure await_workers_ready (communication_context : in ibv_message_bw_interface_h.communication_context_handle) is
      rx_buffer : access ibv_message_bw_interface_h.rx_api_message_buffer;
      num_remaining_workers : natural;
   begin
      num_remaining_workers := ibv_controller_worker_messages_h.NUM_WORKERS;
      while num_remaining_workers > 0 loop
         rx_buffer := ibv_message_bw_interface_h.await_any_rx_message (communication_context);
         declare
            msgs : ibv_controller_worker_messages_h.worker_to_controller_msgs;
            pragma Import (C, msgs);
            for msgs'Address use rx_buffer.data;
            max_name_index : constant Interfaces.C.size_t := Interfaces.C.size_t (msgs.worker_ready.worker_executable_pathname_len) - 1;
         begin
            case ibv_controller_worker_messages_h.controller_worker_msg_ids'Val(rx_buffer.header.message_id) is
               when ibv_controller_worker_messages_h.CW_WORKER_READY =>
                    ada.Text_IO.Put_Line ("worker " & rx_buffer.header.source_instance'Image & " : " &
                                            Interfaces.C.To_Ada (Item => msgs.worker_ready.worker_executable_pathname(0..max_name_index), Trim_Nul => false));

               when others =>
                  raise Ada.Assertions.Assertion_Error with
                    "await_workers_ready unexpected message_id " & rx_buffer.header.message_id'Image;
            end case;
         end;

         ibv_message_bw_interface_h.free_message (rx_buffer);
         num_remaining_workers := num_remaining_workers - 1;
      end loop;
   end await_workers_ready;

   -- @brief Send a message to all workers to tell them to shutdown
   procedure shutdown_workers (paths_to_workers : in paths_to_workers_array) is
      tx_buffer : access ibv_message_bw_interface_h.tx_api_message_buffer;
   begin
      for node_id in all_workers loop
         tx_buffer := ibv_message_bw_interface_h.get_send_buffer (paths_to_workers (node_id));
         declare
            msgs : ibv_controller_worker_messages_h.controller_to_worker_msgs;
            pragma Import (C, msgs);
            for msgs'Address use tx_buffer.data;
         begin
            msgs.request_shutdown.num_requests_per_worker := (others => 0);
            tx_buffer.header.message_id := ibv_controller_worker_messages_h.controller_worker_msg_ids'Pos
              (ibv_controller_worker_messages_h.CW_REQUEST_SHUTDOWN);
            tx_buffer.header.message_length := msgs.request_shutdown'Size / 8;
            ibv_message_bw_interface_h.send_message (tx_buffer);
         end;
      end loop;
   end shutdown_workers;

   communication_context : ibv_message_bw_interface_h.communication_context_handle;
   paths_to_workers : paths_to_workers_array;
begin
   ibv_controller_worker_messages_h.register_controller_worker_messages;
   communication_context :=
     ibv_message_bw_interface_h.communication_context_initialise (Interfaces.C.int (ibv_controller_worker_messages_h.CONTROLLER_NODE_ID));
   for node_id in all_workers loop
      paths_to_workers(node_id) :=
        ibv_message_bw_interface_h.get_tx_path_handle (context => communication_context,
                                                       destination_node => Interfaces.C.int (node_id),
                                                       instance => ibv_controller_worker_messages_h.controller_worker_path_instances'Pos
                                                         (ibv_controller_worker_messages_h.CONTROLLER_TO_WORKER_PATH));
   end loop;

   await_workers_ready (communication_context);
   shutdown_workers (paths_to_workers);
   ibv_message_bw_interface_h.communication_context_finalise (communication_context);
end Ibv_Controller_Process_Main;
