pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
limited with x86_64_linux_gnu_bits_timex_h;

package x86_64_linux_gnu_bits_time_h is

   --  unsupported macro: CLOCKS_PER_SEC ((clock_t) 1000000)
   CLOCK_REALTIME : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/time.h:61

   CLOCK_MONOTONIC : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/time.h:63

   CLOCK_PROCESS_CPUTIME_ID : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/time.h:65

   CLOCK_THREAD_CPUTIME_ID : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/time.h:67

   CLOCK_MONOTONIC_RAW : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/time.h:69

   CLOCK_REALTIME_COARSE : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/time.h:71

   CLOCK_MONOTONIC_COARSE : constant := 6;  --  /usr/include/x86_64-linux-gnu/bits/time.h:73

   CLOCK_BOOTTIME : constant := 7;  --  /usr/include/x86_64-linux-gnu/bits/time.h:75

   CLOCK_REALTIME_ALARM : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/time.h:77

   CLOCK_BOOTTIME_ALARM : constant := 9;  --  /usr/include/x86_64-linux-gnu/bits/time.h:79

   CLOCK_TAI : constant := 11;  --  /usr/include/x86_64-linux-gnu/bits/time.h:81

   TIMER_ABSTIME : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/time.h:84

   type timeval is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:32
      tv_usec : aliased x86_64_linux_gnu_bits_types_h.uu_suseconds_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:33
   end record;
   pragma Convention (C_Pass_By_Copy, timeval);  -- /usr/include/x86_64-linux-gnu/bits/time.h:30

   function clock_adjtime (uu_clock_id : x86_64_linux_gnu_bits_types_h.uu_clockid_t; uu_utx : access x86_64_linux_gnu_bits_timex_h.timex) return int;  -- /usr/include/x86_64-linux-gnu/bits/time.h:93
   pragma Import (C, clock_adjtime, "clock_adjtime");

end x86_64_linux_gnu_bits_time_h;
