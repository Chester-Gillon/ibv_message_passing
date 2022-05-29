-- @file coverage_for_ada_task.adb
-- @date 28 May 2022
-- @author Chester Gillon
-- @brief Example program to test getting coverage for an Ada task

with Ada.Text_IO;
with Ada.Command_Line;

procedure Coverage_For_Ada_Task is
   generic
      Name : String;
   package Generic_Name is
      procedure Display_Name;
   end Generic_Name;

   package body Generic_Name is
      procedure Display_Name is
      begin
         Ada.Text_IO.Put_Line ("My name is " & Name);
      end Display_Name;
   end Generic_Name;

   task Print_Task is
      entry Print;
      entry Finish;
   end Print_Task;

   task body Print_Task is
      Finished : Boolean := False;
   begin
      while not Finished
      loop
         select
            accept Print do
               Ada.Text_IO.Put_Line ("In task Print_Task");
            end Print;

         or
            accept Finish do
               Finished := True;
            end Finish;
         or
            terminate;
         end select;
      end loop;
   end Print_Task;

   package Package_A is new Generic_Name ("A");
   package Package_B is new Generic_Name ("B");
   package Package_C is new Generic_Name ("C");
begin
   Package_A.Display_Name;
   if Ada.Command_Line.Argument_Count > 0 then
      -- If any command line arguments are passed call the generic function 'B'. This is to test getting coverage
      -- from only certain generic packages.
      Package_B.Display_Name;
   end if;
   for index in 1..3
   loop
      Print_Task.Print;
   end loop;
   Package_C.Display_Name;
   Ada.Text_IO.Put_Line ("In main");

   -- If any command line arguments are passed cause the task to execute the Finish entry point, otherwise leave to the
   -- terminate alternate. This is to demonstrate getting coverage on only certain task entry points.
   if Ada.Command_Line.Argument_Count > 0 then
      Print_Task.Finish;
   end if;
end Coverage_For_Ada_Task;
