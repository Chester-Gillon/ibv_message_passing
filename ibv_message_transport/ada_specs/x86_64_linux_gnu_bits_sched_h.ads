pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package x86_64_linux_gnu_bits_sched_h is

   SCHED_OTHER : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:28
   SCHED_FIFO : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:29
   SCHED_RR : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:30

   SCHED_BATCH : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:32
   SCHED_IDLE : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:33

   SCHED_RESET_ON_FORK : constant := 16#40000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:35

   CSIGNAL : constant := 16#000000ff#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:40
   CLONE_VM : constant := 16#00000100#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:41
   CLONE_FS : constant := 16#00000200#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:42
   CLONE_FILES : constant := 16#00000400#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:43
   CLONE_SIGHAND : constant := 16#00000800#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:44
   CLONE_PTRACE : constant := 16#00002000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:45
   CLONE_VFORK : constant := 16#00004000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:46

   CLONE_PARENT : constant := 16#00008000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:48

   CLONE_THREAD : constant := 16#00010000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:50
   CLONE_NEWNS : constant := 16#00020000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:51
   CLONE_SYSVSEM : constant := 16#00040000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:52
   CLONE_SETTLS : constant := 16#00080000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:53
   CLONE_PARENT_SETTID : constant := 16#00100000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:54

   CLONE_CHILD_CLEARTID : constant := 16#00200000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:56

   CLONE_DETACHED : constant := 16#00400000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:58
   CLONE_UNTRACED : constant := 16#00800000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:59

   CLONE_CHILD_SETTID : constant := 16#01000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:61

   CLONE_NEWUTS : constant := 16#04000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:63
   CLONE_NEWIPC : constant := 16#08000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:64
   CLONE_NEWUSER : constant := 16#10000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:65
   CLONE_NEWPID : constant := 16#20000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:66
   CLONE_NEWNET : constant := 16#40000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:67
   CLONE_IO : constant := 16#80000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:68

   type sched_param is record
      uu_sched_priority : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:74
   end record;
   pragma Convention (C_Pass_By_Copy, sched_param);  -- /usr/include/x86_64-linux-gnu/bits/sched.h:72

   function clone
     (uu_fn : access function (arg1 : System.Address) return int;
      uu_child_stack : System.Address;
      uu_flags : int;
      uu_arg : System.Address  -- , ...
      ) return int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:81
   pragma Import (C, clone, "clone");

   function unshare (uu_flags : int) return int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:85
   pragma Import (C, unshare, "unshare");

   function sched_getcpu return int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:88
   pragma Import (C, sched_getcpu, "sched_getcpu");

   function setns (uu_fd : int; uu_nstype : int) return int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:91
   pragma Import (C, setns, "setns");

   type uu_sched_param is record
      uu_sched_priority : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:105
   end record;
   pragma Convention (C_Pass_By_Copy, uu_sched_param);  -- /usr/include/x86_64-linux-gnu/bits/sched.h:103

   subtype uu_cpu_mask is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:118

   type cpu_set_t_uu_bits_array is array (0 .. 15) of aliased uu_cpu_mask;
   type cpu_set_t is record
      uu_bits : aliased cpu_set_t_uu_bits_array;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:127
   end record;
   pragma Convention (C_Pass_By_Copy, cpu_set_t);  -- /usr/include/x86_64-linux-gnu/bits/sched.h:128

   --  skipped anonymous struct anon_1

   --  skipped func __sched_cpucount

   --  skipped func __sched_cpualloc

   --  skipped func __sched_cpufree

end x86_64_linux_gnu_bits_sched_h;
