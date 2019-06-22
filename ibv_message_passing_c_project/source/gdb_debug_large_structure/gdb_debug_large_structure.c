/*
 * @file gdb_debug_large_structure.c
 * @date 22 Jun 2019
 * @author Chester Gillon
 * @details Created to test in which versions of GDB the bug in https://sourceware.org/bugzilla/show_bug.cgi?id=18394
 *          "struct member address printing is off" exists.
 */

#include <stdlib.h>
#include <stdio.h>

typedef struct
{
    char a[/*0x2000 */   2097152];
    char b[/*0x400 */  536870912];
    char c[/*0x8000 */  33554432];
} large_struct_t;

int main (int argc, char *argv[])
{
    large_struct_t *const data = calloc (sizeof (large_struct_t), 1);

    printf ("data=%p\n", data);
    printf ("&data->a=%p\n", &data->a);
    printf ("&data->b=%p\n", &data->b);
    printf ("&data->c=%p\n", &data->c);

    return 0;
}
