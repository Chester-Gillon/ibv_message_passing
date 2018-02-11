pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;

package ibv_controller_worker_messages_h is

   NUM_WORKERS : constant := 3;  --  ../ibv_controller_worker_messages.h:12

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
      CW_REQUEST_SHUTDOWN);
   pragma Convention (C, controller_worker_msg_ids);  -- ../ibv_controller_worker_messages.h:35

   subtype worker_ready_msg_worker_executable_pathname_array is Interfaces.C.char_array (0 .. 4095);
   type worker_ready_msg is record
      worker_executable_pathname_len : aliased stdint_h.uint32_t;  -- ../ibv_controller_worker_messages.h:41
      worker_executable_pathname : aliased worker_ready_msg_worker_executable_pathname_array;  -- ../ibv_controller_worker_messages.h:43
   end record;
   pragma Convention (C_Pass_By_Copy, worker_ready_msg);  -- ../ibv_controller_worker_messages.h:44

   --  skipped anonymous struct anon_3

   type request_shutdown_msg_num_requests_per_worker_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type request_shutdown_msg is record
      num_requests_per_worker : aliased request_shutdown_msg_num_requests_per_worker_array;  -- ../ibv_controller_worker_messages.h:50
   end record;
   pragma Convention (C_Pass_By_Copy, request_shutdown_msg);  -- ../ibv_controller_worker_messages.h:51

   --  skipped anonymous struct anon_4

   type worker_to_controller_msgs (discr : unsigned := 0) is record
      case discr is
         when others =>
            worker_ready : aliased worker_ready_msg;  -- ../ibv_controller_worker_messages.h:56
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, worker_to_controller_msgs);
   pragma Unchecked_Union (worker_to_controller_msgs);  -- ../ibv_controller_worker_messages.h:57

   --  skipped anonymous struct anon_5

   type controller_to_worker_msgs (discr : unsigned := 0) is record
      case discr is
         when others =>
            request_shutdown : aliased request_shutdown_msg;  -- ../ibv_controller_worker_messages.h:62
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, controller_to_worker_msgs);
   pragma Unchecked_Union (controller_to_worker_msgs);  -- ../ibv_controller_worker_messages.h:63

   --  skipped anonymous struct anon_6

   procedure register_controller_worker_messages;  -- ../ibv_controller_worker_messages.h:65
   pragma Import (C, register_controller_worker_messages, "register_controller_worker_messages");

end ibv_controller_worker_messages_h;
