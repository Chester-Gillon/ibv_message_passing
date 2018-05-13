-- @file ibv_controller_process_main.adb
-- @date 13 May 2018
-- @author Chester Gillon
-- @details Example program created to demonstrate a GDB crash when using:
--          - Eclipse Oxygen 4.7.3a
--          - GNATbench Integration with CDT 2.8.1.20140109
--          - GNAT GPL 2017
--          - GDB 7.10 (in the GNAT GPL installation)
--
--          Steps to recreate:
--          1) When single stepping the program when get to the first statement with msgs in scope, in the Variables
--             window double-click on msgs to try and expand the contents (which fails).
--          2) On the next single step, when msgs is still in scope, GDB crashes with a SISEGV.
with System;
with Interfaces.C;
use Interfaces.C;
with stddef_h;
with ibv_controller_worker_messages_h;

procedure ibv_message_overlay_gdb_test is
   function malloc (size : stddef_h.size_t) return System.Address;
   pragma Import (C, malloc, "malloc");
   procedure free (buffer : System.Address);
   pragma Import (C, free, "free");

   tx_buffer : constant System.Address := malloc (ibv_controller_worker_messages_h.controller_to_worker_msgs'Size / 8);
begin
   declare
      msgs : ibv_controller_worker_messages_h.controller_to_worker_msgs;
      pragma Import (C, msgs);
      for msgs'Address use tx_buffer;
   begin
      msgs.sum_integers.request_id := Interfaces.C.unsigned (10);
      msgs.sum_integers.num_integers_to_sum := 30;
      msgs.sum_integers.integers_to_sum := (others => 16#deaddead#);
     end;
   free (tx_buffer);
end ibv_message_overlay_gdb_test;
