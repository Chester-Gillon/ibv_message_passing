-- @file ibv_controller_process_main.adb
-- @date 11 Feb 2018
-- @author Chester Gillon
-- @details The main for the controller process written in Ada with communicates with
--          worker processing written in C via the ibv_message_transport library.

pragma Restrictions (No_Exceptions);
with ada.Text_IO;
with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with ibv_message_bw_interface_h;
with ibv_controller_worker_messages_h;
with Ada.Assertions;
with Ada.Numerics.Discrete_Random;
with stdint_h;
with Ada.Command_Line;

procedure Ibv_Controller_Process_Main is

   type all_workers is range ibv_controller_worker_messages_h.FIRST_WORKER_NODE_ID .. ibv_controller_worker_messages_h.LAST_WORKER_NODE_ID;
   type paths_to_workers_array is array (all_workers) of
      ibv_message_bw_interface_h.tx_message_context_handle;

   package next_worker_random is new Ada.Numerics.Discrete_Random (all_workers);

   type random_int_range is range 0..65535;
   package data_to_sum_random is new Ada.Numerics.Discrete_Random (random_int_range);

   type num_ints_range is range 1..ibv_controller_worker_messages_h.MAX_INTEGERS_TO_SUM;
   package num_ints_to_sum_random is new Ada.Numerics.Discrete_Random (num_ints_range);

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
                    ada.Text_IO.Put_Line ("worker " & Interfaces.C.Unsigned'Image (rx_buffer.header.source_instance) & " : " &
                                            Interfaces.C.To_Ada (Item => msgs.worker_ready.worker_executable_pathname(0..max_name_index), Trim_Nul => false));

               when others =>
                  ibv_message_bw_interface_h.check_assert (assertion => false,
                                                           message => Interfaces.C.Strings.New_String ("await_workers_ready unexpected message"));
            end case;
         end;

         ibv_message_bw_interface_h.free_message (rx_buffer);
         num_remaining_workers := num_remaining_workers - 1;
      end loop;
   end await_workers_ready;

   -- @brief Process all available CW_SUM_RESULT replies from the workers
   -- @detail This verifies that the expected sum has been returned, by using the returned
   --         request_id to seed the same same random number generators for the number and
   --         data values sent to calculate an expected sum.
   --         This avoids needing to keep track of the outstanding request messages.
   procedure process_sum_result_replies (communication_context : in ibv_message_bw_interface_h.communication_context_handle;
                                         num_outstanding_replies : in out natural) is
      rx_buffer : access ibv_message_bw_interface_h.rx_api_message_buffer;
   begin
      loop
         rx_buffer := ibv_message_bw_interface_h.poll_rx_paths (communication_context);
         if rx_buffer /= null then
            declare
               msgs : ibv_controller_worker_messages_h.worker_to_controller_msgs;
               pragma Import (C, msgs);
               for msgs'Address use rx_buffer.data;
               expected_sum : natural;
               data_generator : data_to_sum_random.Generator;
               num_ints_generator : num_ints_to_sum_random.Generator;
            begin
               case ibv_controller_worker_messages_h.controller_worker_msg_ids'Val(rx_buffer.header.message_id) is
                  when ibv_controller_worker_messages_h.CW_SUM_RESULT =>
                     data_to_sum_random.Reset (Gen => data_generator,
                                               Initiator => Standard.Integer(msgs.sum_result.request_id));
                     num_ints_to_sum_random.Reset (Gen => num_ints_generator,
                                                   Initiator => Standard.Integer(msgs.sum_result.request_id));
                     expected_sum := 0;
                     for ints_to_sum in 1 .. num_ints_to_sum_random.Random (num_ints_generator) loop
                        expected_sum := expected_sum + natural (data_to_sum_random.Random (data_generator));
                     end loop;
                     if expected_sum /= natural (msgs.sum_result.sum) then
                        ibv_message_bw_interface_h.check_assert (assertion => false,
                                                                 message => Interfaces.C.Strings.New_String ("process_sum_result_replies wrong sum"));
                     end if;
                     num_outstanding_replies := num_outstanding_replies - 1;
                     ibv_message_bw_interface_h.free_message (rx_buffer);

                  when others =>
                     ibv_message_bw_interface_h.check_assert (assertion => false,
                                                              message => Interfaces.C.Strings.New_String ("process_sum_result_replies unexpected message"));
               end case;
            end;

         else
            exit;
         end if;
      end loop;
   end process_sum_result_replies;

   -- @brief Send a CW_SUM_INTEGERS message to a worker.
   -- @details The number of integers to sum, and their values, are generated with a psuedo-random
   --          pattern. This acts as a test of variable length messages.
   procedure send_sum_integers (tx_buffer : access ibv_message_bw_interface_h.tx_api_message_buffer;
                                request_id : in Natural) is
      msgs : ibv_controller_worker_messages_h.controller_to_worker_msgs;
      pragma Import (C, msgs);
      for msgs'Address use tx_buffer.data;
      data_generator : data_to_sum_random.Generator;
      num_ints_generator : num_ints_to_sum_random.Generator;
   begin
      data_to_sum_random.Reset (Gen => data_generator, Initiator => request_id);
      num_ints_to_sum_random.Reset (Gen => num_ints_generator, Initiator => request_id);
      tx_buffer.header.message_id := ibv_controller_worker_messages_h.controller_worker_msg_ids'Pos
        (ibv_controller_worker_messages_h.CW_SUM_INTEGERS);
      msgs.sum_integers.request_id := Interfaces.C.unsigned (request_id);
      msgs.sum_integers.num_integers_to_sum :=
        Interfaces.C.unsigned (num_ints_to_sum_random.Random (num_ints_generator));
      for data_index in 0 .. (msgs.sum_integers.num_integers_to_sum - 1) loop
         msgs.sum_integers.integers_to_sum(Integer(data_index)) :=
           Interfaces.C.unsigned (data_to_sum_random.Random (data_generator));
      end loop;
      tx_buffer.header.message_length := msgs.sum_integers.integers_to_sum'Position +
        ((msgs.sum_integers.num_integers_to_sum * msgs.sum_integers.integers_to_sum(0)'Size) / 8);
      ibv_message_bw_interface_h.send_message (tx_buffer);
   end send_sum_integers;

   -- @details Perform a test by sending a number of CW_SUM_INTEGERS messages to the workers,
   --          and waiting for and then checking the CW_SUM_RESULT reply messages.
   procedure perform_sum_integer_test (communication_context : in ibv_message_bw_interface_h.communication_context_handle;
                                       paths_to_workers : in paths_to_workers_array;
                                       num_requests_per_worker : in out ibv_controller_worker_messages_h.request_shutdown_msg_num_requests_per_worker_array) is
      num_unsent_requests : natural := 10000;
      num_outstanding_replies : natural := 0;
      next_request_id : natural := 1;
      next_worker_generator : next_worker_random.Generator;
      worker_node_id : all_workers;
      tx_buffer : access ibv_message_bw_interface_h.tx_api_message_buffer;
   begin
      -- Send requests to the workers, and process the reponses, until all requests
      -- have been sent
      worker_node_id := next_worker_random.Random (next_worker_generator);
      while num_unsent_requests > 0 loop
         process_sum_result_replies (communication_context => communication_context,
                                     num_outstanding_replies => num_outstanding_replies);
         tx_buffer := ibv_message_bw_interface_h.get_send_buffer_no_wait (paths_to_workers(worker_node_id));
         if tx_buffer /= null then
            declare
               worker_node_index : constant Natural := natural(worker_node_id) - natural(all_workers'First);
            begin
               send_sum_integers (tx_buffer => tx_buffer, request_id => next_request_id);
               num_unsent_requests := num_unsent_requests - 1;
               num_outstanding_replies := num_outstanding_replies + 1;
               num_requests_per_worker(worker_node_index) := num_requests_per_worker(worker_node_index) + 1;
               worker_node_id := next_worker_random.Random (next_worker_generator);
               next_request_id := next_request_id + 1;
            end;
         end if;
      end loop;

      -- Wait for outstanding replies from the workers
      while num_outstanding_replies > 0 loop
         process_sum_result_replies (communication_context => communication_context,
                                     num_outstanding_replies => num_outstanding_replies);
      end loop;
   end perform_sum_integer_test;

   -- @brief Send a message to all workers to tell them to shutdown
   procedure shutdown_workers (paths_to_workers : in paths_to_workers_array;
                               num_requests_per_worker : in ibv_controller_worker_messages_h.request_shutdown_msg_num_requests_per_worker_array) is
      tx_buffer : access ibv_message_bw_interface_h.tx_api_message_buffer;
   begin
      for node_id in all_workers loop
         tx_buffer := ibv_message_bw_interface_h.get_send_buffer (paths_to_workers (node_id));
         declare
            msgs : ibv_controller_worker_messages_h.controller_to_worker_msgs;
            pragma Import (C, msgs);
            for msgs'Address use tx_buffer.data;
         begin
            msgs.request_shutdown.num_requests_per_worker := num_requests_per_worker;
            tx_buffer.header.message_id := ibv_controller_worker_messages_h.controller_worker_msg_ids'Pos
              (ibv_controller_worker_messages_h.CW_REQUEST_SHUTDOWN);
            tx_buffer.header.message_length := msgs.request_shutdown'Size / 8;
            ibv_message_bw_interface_h.send_message (tx_buffer);
         end;
      end loop;
   end shutdown_workers;

   communication_context : ibv_message_bw_interface_h.communication_context_handle;
   paths_to_workers : paths_to_workers_array;
   num_requests_per_worker : ibv_controller_worker_messages_h.request_shutdown_msg_num_requests_per_worker_array := (others => 0);
   controller_and_workers_on_separate_pcs : Interfaces.C.Extensions.bool := false;
begin
   -- Initialise, establishing connection with the workers
   if Ada.Command_Line.Argument_Count >= 1 then
      controller_and_workers_on_separate_pcs := Interfaces.C.Extensions.bool'Value(Ada.Command_Line.Argument (1));
   end if;
   ibv_controller_worker_messages_h.register_controller_worker_messages (controller_and_workers_on_separate_pcs);
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

   -- Run the test, communicating with the workers
   perform_sum_integer_test (communication_context => communication_context, paths_to_workers => paths_to_workers,
                             num_requests_per_worker => num_requests_per_worker);

   -- Finalise, shuting down the workers
   shutdown_workers (paths_to_workers => paths_to_workers, num_requests_per_worker => num_requests_per_worker);
   ibv_message_bw_interface_h.communication_context_finalise (communication_context);
end Ibv_Controller_Process_Main;
