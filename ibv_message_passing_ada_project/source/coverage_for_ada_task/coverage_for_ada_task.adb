-- @file coverage_for_ada_task.adb
-- @date 28 May 2022
-- @author Chester Gillon
-- @brief Example program to test getting coverage for an Ada task

with Ada.Text_IO;

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
   end Print_Task;

   task body Print_Task is
   begin
      loop
         select
            accept Print do
               Ada.Text_IO.Put_Line ("In task Print_Task");
            end Print;
         or
            terminate;
         end select;
      end loop;
   end Print_Task;

   package Package_A is new Generic_Name ("A");
   package Package_B is new Generic_Name ("B");
begin
   Package_A.Display_Name;
   for index in 1..3
   loop
      Print_Task.Print;
   end loop;
   Package_B.Display_Name;
   Ada.Text_IO.Put_Line ("In main");
end Coverage_For_Ada_Task;
