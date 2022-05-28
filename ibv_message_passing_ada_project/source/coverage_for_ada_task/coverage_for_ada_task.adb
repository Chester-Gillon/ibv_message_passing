-- @file coverage_for_ada_task.adb
-- @date 28 May 2022
-- @author Chester Gillon
-- @brief Example program to test getting coverage for an Ada task

with Ada.Text_IO;

procedure Coverage_For_Ada_Task is
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
begin
   for index in 1..3
   loop
      Print_Task.Print;
   end loop;
   Ada.Text_IO.Put_Line ("In main");
end Coverage_For_Ada_Task;
