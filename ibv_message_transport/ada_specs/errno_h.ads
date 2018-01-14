pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package errno_h is

   program_invocation_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/errno.h:54
   pragma Import (C, program_invocation_name, "program_invocation_name");

   program_invocation_short_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/errno.h:54
   pragma Import (C, program_invocation_short_name, "program_invocation_short_name");

   subtype error_t is int;  -- /usr/include/errno.h:68

end errno_h;
