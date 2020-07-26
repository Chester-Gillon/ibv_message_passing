-- @file shared_file_io.adb
-- @date 26 July 2020
-- @author Chester Gillon
-- @brief Example program to attempt to open the same file more than once in a program using the GNAT RTS

with Ada.Text_IO;
with Ada.Exceptions;

procedure Shared_File_Io is
   In_File_A : Ada.Text_IO.File_Type;
   In_File_B : Ada.Text_IO.File_Type;
   Test_Filename : constant string := "/proc/self/maps";
begin
   Ada.Text_IO.Put_Line ("In_File_A.Is_Open => " & Ada.Text_IO.Is_Open (File => In_File_A)'Image);
   Ada.Text_IO.Put_Line ("In_File_B.Is_Open => " & Ada.Text_IO.Is_Open (File => In_File_B)'Image);

   -- Initial attempt to open the same file twice, with no Form string provided in the Open call.
   -- From https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/the_implementation_of_standard_i_o.html#shared-files :
   -- - In the absence of a shared=xxx form parameter, an attempt to open two or more files with the same full name is
   --   considered an error and is not supported. The exception Use_Error will be raised.
   --   Note that a file that is not explicitly closed by the program remains open until the program terminates.
   begin
      Ada.Text_IO.Open (File => In_File_A, Mode => Ada.Text_IO.In_File, Name => Test_Filename);
      Ada.Text_IO.Put_Line ("In_File_A Form => " & Ada.Text_IO.Form (File => In_File_A));
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line ("Open In_File_A : " & Ada.Exceptions.Exception_Name (The_Error) & " " &
                                 Ada.Exceptions.Exception_Name (The_Error));
   end;

   begin
      Ada.Text_IO.Open (File => In_File_B, Mode => Ada.Text_IO.In_File, Name => Test_Filename);
      Ada.Text_IO.Put_Line ("In_File_B Form => " & Ada.Text_IO.Form (File => In_File_B));
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line ("Open In_File_B : " & Ada.Exceptions.Exception_Name (The_Error) & " " &
                                 Ada.Exceptions.Exception_Message (The_Error));
   end;

   Ada.Text_IO.Put_Line ("In_File_A.Is_Open => " & Ada.Text_IO.Is_Open (File => In_File_A)'Image);
   Ada.Text_IO.Put_Line ("In_File_B.Is_Open => " & Ada.Text_IO.Is_Open (File => In_File_B)'Image);

   if Ada.Text_IO.Is_Open (File => In_File_A) then
      Ada.Text_IO.Close (File => In_File_A);
   end if;
   if Ada.Text_IO.Is_Open (File => In_File_B) then
      Ada.Text_IO.Close (File => In_File_B);
   end if;

   -- Another attempt to open the same file twice, but using "shared=no" as the Form string to cause each file
   -- to opened with its own separate stream identifier. Since the files are only read by the program this should
   -- avoid any conflicts.
   --
   -- Running "ls -l /proc/`pgrep shared_file_io`/fd" shows these two Open calls each open their own file descriptor,
   -- meaning the two streams are independent.
   begin
      Ada.Text_IO.Open (File => In_File_A, Mode => Ada.Text_IO.In_File, Name => Test_Filename, Form => "shared=no");
      Ada.Text_IO.Put_Line ("In_File_A Form => " & Ada.Text_IO.Form (File => In_File_A));
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line ("Open In_File_A : " & Ada.Exceptions.Exception_Name (The_Error) & " " &
                                 Ada.Exceptions.Exception_Name (The_Error));
   end;

   begin
      Ada.Text_IO.Open (File => In_File_B, Mode => Ada.Text_IO.In_File, Name => Test_Filename, Form => "shared=no");
      Ada.Text_IO.Put_Line ("In_File_B Form => " & Ada.Text_IO.Form (File => In_File_B));
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line ("Open In_File_B : " & Ada.Exceptions.Exception_Name (The_Error) & " " &
                                 Ada.Exceptions.Exception_Message (The_Error));
   end;

   Ada.Text_IO.Put_Line ("In_File_A.Is_Open => " & Ada.Text_IO.Is_Open (File => In_File_A)'Image);
   Ada.Text_IO.Put_Line ("In_File_B.Is_Open => " & Ada.Text_IO.Is_Open (File => In_File_B)'Image);

   Ada.Text_IO.Close (File => In_File_A);
   Ada.Text_IO.Close (File => In_File_B);
end Shared_File_Io;
