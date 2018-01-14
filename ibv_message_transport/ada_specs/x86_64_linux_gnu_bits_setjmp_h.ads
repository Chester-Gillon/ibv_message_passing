pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_setjmp_h is

   type uu_jmp_buf is array (0 .. 7) of aliased long;  -- /usr/include/x86_64-linux-gnu/bits/setjmp.h:31

end x86_64_linux_gnu_bits_setjmp_h;
