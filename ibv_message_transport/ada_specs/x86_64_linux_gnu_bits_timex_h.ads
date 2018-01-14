pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with x86_64_linux_gnu_bits_time_h;

package x86_64_linux_gnu_bits_timex_h is

   ADJ_OFFSET : constant := 16#0001#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:56
   ADJ_FREQUENCY : constant := 16#0002#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:57
   ADJ_MAXERROR : constant := 16#0004#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:58
   ADJ_ESTERROR : constant := 16#0008#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:59
   ADJ_STATUS : constant := 16#0010#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:60
   ADJ_TIMECONST : constant := 16#0020#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:61
   ADJ_TAI : constant := 16#0080#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:62
   ADJ_SETOFFSET : constant := 16#0100#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:63
   ADJ_MICRO : constant := 16#1000#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:64
   ADJ_NANO : constant := 16#2000#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:65
   ADJ_TICK : constant := 16#4000#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:66
   ADJ_OFFSET_SINGLESHOT : constant := 16#8001#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:67
   ADJ_OFFSET_SS_READ : constant := 16#a001#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:68
   --  unsupported macro: MOD_OFFSET ADJ_OFFSET
   --  unsupported macro: MOD_FREQUENCY ADJ_FREQUENCY
   --  unsupported macro: MOD_MAXERROR ADJ_MAXERROR
   --  unsupported macro: MOD_ESTERROR ADJ_ESTERROR
   --  unsupported macro: MOD_STATUS ADJ_STATUS
   --  unsupported macro: MOD_TIMECONST ADJ_TIMECONST
   --  unsupported macro: MOD_CLKB ADJ_TICK
   --  unsupported macro: MOD_CLKA ADJ_OFFSET_SINGLESHOT
   --  unsupported macro: MOD_TAI ADJ_TAI
   --  unsupported macro: MOD_MICRO ADJ_MICRO
   --  unsupported macro: MOD_NANO ADJ_NANO

   STA_PLL : constant := 16#0001#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:85
   STA_PPSFREQ : constant := 16#0002#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:86
   STA_PPSTIME : constant := 16#0004#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:87
   STA_FLL : constant := 16#0008#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:88

   STA_INS : constant := 16#0010#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:90
   STA_DEL : constant := 16#0020#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:91
   STA_UNSYNC : constant := 16#0040#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:92
   STA_FREQHOLD : constant := 16#0080#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:93

   STA_PPSSIGNAL : constant := 16#0100#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:95
   STA_PPSJITTER : constant := 16#0200#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:96
   STA_PPSWANDER : constant := 16#0400#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:97
   STA_PPSERROR : constant := 16#0800#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:98

   STA_CLOCKERR : constant := 16#1000#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:100
   STA_NANO : constant := 16#2000#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:101
   STA_MODE : constant := 16#4000#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:102
   STA_CLK : constant := 16#8000#;  --  /usr/include/x86_64-linux-gnu/bits/timex.h:103
   --  unsupported macro: STA_RONLY (STA_PPSSIGNAL | STA_PPSJITTER | STA_PPSWANDER | STA_PPSERROR | STA_CLOCKERR | STA_NANO | STA_MODE | STA_CLK)

   type timex is record
      modes : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:27
      offset : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:28
      freq : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:29
      maxerror : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:30
      esterror : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:31
      status : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:32
      c_constant : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:33
      precision : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:34
      tolerance : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:35
      time : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:36
      tick : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:37
      ppsfreq : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:38
      jitter : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:39
      shift : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:40
      stabil : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:41
      jitcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:42
      calcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:43
      errcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:44
      stbcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:45
      tai : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:47
      field_21 : aliased int;
      field_22 : aliased int;
      field_23 : aliased int;
      field_24 : aliased int;
      field_25 : aliased int;
      field_26 : aliased int;
      field_27 : aliased int;
      field_28 : aliased int;
      field_29 : aliased int;
      field_30 : aliased int;
      field_31 : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, timex);  -- /usr/include/x86_64-linux-gnu/bits/timex.h:25

end x86_64_linux_gnu_bits_timex_h;
