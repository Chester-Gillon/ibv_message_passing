pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;

package ibv_controller_worker_messages_h is

   NUM_WORKERS : constant := 3;  --  ../ibv_controller_worker_messages.h:12

   MAX_INTEGERS_TO_SUM : constant := 2048;  --  ../ibv_controller_worker_messages.h:58

   subtype controller_worker_node_ids is unsigned;
   CONTROLLER_NODE_ID : constant controller_worker_node_ids := 0;
   FIRST_WORKER_NODE_ID : constant controller_worker_node_ids := 1;
   LAST_WORKER_NODE_ID : constant controller_worker_node_ids := 3;  -- ../ibv_controller_worker_messages.h:20

   type controller_worker_path_instances is 
     (CONTROLLER_TO_WORKER_PATH,
      WORKER_TO_CONTROLLER_PATH);
   pragma Convention (C, controller_worker_path_instances);  -- ../ibv_controller_worker_messages.h:27

   type controller_worker_msg_ids is 
     (CW_WORKER_READY,
      CW_REQUEST_SHUTDOWN,
      CW_SUM_INTEGERS,
      CW_SUM_RESULT);
   pragma Convention (C, controller_worker_msg_ids);  -- ../ibv_controller_worker_messages.h:39

   subtype worker_ready_msg_worker_executable_pathname_array is Interfaces.C.char_array (0 .. 4095);
   type worker_ready_msg is record
      worker_executable_pathname_len : aliased stdint_h.uint32_t;  -- ../ibv_controller_worker_messages.h:45
      worker_executable_pathname : aliased worker_ready_msg_worker_executable_pathname_array;  -- ../ibv_controller_worker_messages.h:47
   end record;
   pragma Convention (C_Pass_By_Copy, worker_ready_msg);  -- ../ibv_controller_worker_messages.h:48

   --  skipped anonymous struct anon_3

   type request_shutdown_msg_num_requests_per_worker_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type request_shutdown_msg is record
      num_requests_per_worker : aliased request_shutdown_msg_num_requests_per_worker_array;  -- ../ibv_controller_worker_messages.h:54
   end record;
   pragma Convention (C_Pass_By_Copy, request_shutdown_msg);  -- ../ibv_controller_worker_messages.h:55

   --  skipped anonymous struct anon_4

   type sum_integers_msg_integers_to_sum_array is array (0 .. 2047) of aliased stdint_h.uint32_t;
   type sum_integers_msg is record
      request_id : aliased stdint_h.uint32_t;  -- ../ibv_controller_worker_messages.h:62
      num_integers_to_sum : aliased stdint_h.uint32_t;  -- ../ibv_controller_worker_messages.h:64
      integers_to_sum : aliased sum_integers_msg_integers_to_sum_array;  -- ../ibv_controller_worker_messages.h:66
   end record;
   pragma Convention (C_Pass_By_Copy, sum_integers_msg);  -- ../ibv_controller_worker_messages.h:67

   --  skipped anonymous struct anon_5

   type sum_result_msg is record
      request_id : aliased stdint_h.uint32_t;  -- ../ibv_controller_worker_messages.h:73
      sum : aliased stdint_h.uint32_t;  -- ../ibv_controller_worker_messages.h:75
   end record;
   pragma Convention (C_Pass_By_Copy, sum_result_msg);  -- ../ibv_controller_worker_messages.h:76

   --  skipped anonymous struct anon_6

   type worker_to_controller_msgs (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            worker_ready : aliased worker_ready_msg;  -- ../ibv_controller_worker_messages.h:81
         when others =>
            sum_result : aliased sum_result_msg;  -- ../ibv_controller_worker_messages.h:82
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, worker_to_controller_msgs);
   pragma Unchecked_Union (worker_to_controller_msgs);  -- ../ibv_controller_worker_messages.h:83

   --  skipped anonymous struct anon_7

   type controller_to_worker_msgs (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            request_shutdown : aliased request_shutdown_msg;  -- ../ibv_controller_worker_messages.h:88
         when others =>
            sum_integers : aliased sum_integers_msg;  -- ../ibv_controller_worker_messages.h:89
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, controller_to_worker_msgs);
   pragma Unchecked_Union (controller_to_worker_msgs);  -- ../ibv_controller_worker_messages.h:90

   --  skipped anonymous struct anon_8

   procedure register_controller_worker_messages;  -- ../ibv_controller_worker_messages.h:92
   pragma Import (C, register_controller_worker_messages, "register_controller_worker_messages");

end ibv_controller_worker_messages_h;
