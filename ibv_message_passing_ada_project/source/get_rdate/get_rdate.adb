-- @file get_rdate.adb
-- @date 7 July 2019
-- @author Chester Gillon
-- @brief Example program using GNAT.sockets to get the date from a RFC 868 TCP protocol server
-- @details The output as seconds since the Linux epoch can be verified by running:
--          $ date -d @`./get_rdate sandy-ubuntu`
--          Sun  7 Jul 19:42:14 BST 2019

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with GNAT.Sockets;

procedure Get_RDate is
   Rdate_Client : GNAT.Sockets.Socket_Type;
   Address : GNAT.Sockets.Sock_Addr_Type;
   Channel : GNAT.Sockets.Stream_Access;
   Rdate_Port : GNAT.Sockets.Port_Type := 37;

   -- Define am array to receive the rdate time as a big-endian 32-bit number of seconds
   -- since the epoch of 00:00 (midnight) 1 January 1900 GMT
   type Rdate_Time_Packet is Array (0..3) of Interfaces.Unsigned_8;
   Raw_Time : Rdate_Time_Packet;

   Host_Rdate_Time : Interfaces.Unsigned_32;

   -- From RFC 868
   Rdate_To_Linux_Epoch_Seconds : constant := 2208988800;

   Time_Since_Linux_Epoch : Interfaces.Unsigned_32;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: " & Ada.Command_Line.Command_Name & "get_rdate <rdate_server>");
      return;
   end if;

   declare
      Rdate_Server : constant String := Ada.Command_Line.Argument (1);
   begin
      -- Connect to the specified rdate server and retrieve a packet with the time
      GNAT.Sockets.Initialize;
      GNAT.Sockets.Create_Socket (Rdate_Client);
      Address.Addr := GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Rdate_Server));
      Address.Port := Rdate_Port;
      GNAT.Sockets.Connect_Socket (Rdate_Client, Address);
      Channel := GNAT.Sockets.Stream (Rdate_Client);
      Rdate_Time_Packet'Read (Channel, Raw_Time);
      GNAT.Sockets.Close_Socket (Rdate_Client);

      -- Convert the big-endian received rdate
      Host_Rdate_Time := Interfaces.Shift_Left (Interfaces.Unsigned_32 (Raw_Time(0)), 24) +
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Raw_Time(1)), 16) +
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Raw_Time(2)), 8) +
        Interfaces.Unsigned_32 (Raw_Time(3));

      -- Display the time as the number of seconds since the Linux epoch.
      Time_Since_Linux_Epoch := Host_Rdate_Time - Rdate_To_Linux_Epoch_Seconds;
      declare
         Raw_Image : constant String := Interfaces.Unsigned_32'Image (Time_Since_Linux_Epoch);
      begin
         Ada.Text_Io.Put (Ada.Strings.Fixed.Trim (Raw_Image, Ada.Strings.Left));
      end;
   end;
end Get_Rdate;
