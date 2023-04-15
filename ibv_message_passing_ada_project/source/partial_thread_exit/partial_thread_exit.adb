-- @file partial_thread_exit.adb
-- @date 15 April 2023
-- @author Chester Gillon
-- @brief Program to test where the main thread of a process exits, leaving another thread blocked.
-- @details The main thread opens a file to test scanning for references to the file before and after the main thread exits.

with Ada.Text_IO;
with Interfaces.C;
with System;

procedure Partial_Thread_Exit is
   task Blocker_Task is
      entry Identify;
      entry Quit;
   end Blocker_Task;

   subtype pid_t is Interfaces.C.int;

   function getpid return pid_t;
   pragma import (C, getpid, "getpid");

   function Sys_Call (number : in Interfaces.C.long) return Interfaces.C.long;
   pragma import (C, Sys_Call, "syscall");

   function gettid return pid_t is
      NR_gettid : constant := 186;
   begin
      return pid_t (Sys_Call (NR_gettid));
   end gettid;

   -- Exit just the calling thread.
   -- As noted by https://stackoverflow.com/a/46903734/4207678 "Syscall implementation of exit()" calling the libc exit()
   -- uses the exit_group syscall which causes all threads in the process to exit.
   procedure Exit_Thread is
      NR_exit : constant := 60;
      Ignored : Interfaces.C.long;
   begin
      Ignored := Sys_Call (NR_exit);
   end Exit_Thread;

   task body Blocker_Task is
      Finished : Boolean := False;
   begin
      while not Finished
      loop
         select
            accept Identify do
               Ada.Text_IO.Put_Line ("Blocker_Task tid" & pid_t'Image (gettid) & " running in pid" & pid_t'Image (getpid));
            end Identify;

         or
            accept Quit do
               Finished := True;
            end Quit;

         or
            terminate;
         end select;
      end loop;
   end Blocker_Task;

   Reference_Pathname : constant string := "/dev/shm/IBV_MESSAGE_BW_IN_USE";
   Reference_File : Ada.Text_IO.File_Type;

begin
   Ada.Text_IO.Create (File => Reference_File, Mode => Ada.Text_IO.Append_File, Name => Reference_Pathname);

   Blocker_Task.Identify;
   Ada.Text_IO.Put_Line ("Main thread in pid" & pid_t'Image (getpid) & " - press return to exit main thread only");
   declare
      Input_Line : constant string :=  Ada.Text_IO.Get_Line;
   begin

      -- This causes the thread to exit with a Sys_Calling meaning the Ada tasking doesn't get a chance to terminate
      -- the Blocker_Task
      Ada.Text_IO.Put_Line ("Main thread exiting...");
      Exit_Thread;
   end;
end Partial_Thread_Exit;
