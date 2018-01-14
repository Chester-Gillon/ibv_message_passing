pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   --  unsupported macro: NULL __null
   --  arg-macro: procedure offsetof (TYPE, MEMBER)
   --    __builtin_offsetof (TYPE, MEMBER)
   subtype ptrdiff_t is long;  -- /usr/gnat/lib/gcc/x86_64-pc-linux-gnu/6.3.1/include/stddef.h:149

   subtype size_t is unsigned_long;  -- /usr/gnat/lib/gcc/x86_64-pc-linux-gnu/6.3.1/include/stddef.h:216

end stddef_h;
