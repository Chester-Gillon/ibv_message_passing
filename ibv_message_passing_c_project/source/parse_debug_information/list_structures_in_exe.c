/*
 * @file list_structures_in_exe.c
 * @date 11 Oct 2020
 * @author Chester Gillon
 */

#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <dwarf.h>
#include <elfutils/libdw.h>


/**
 * @brief Get the size of the underlying type of a die
 * @detail Follows type references until find one with a size.
 *         E.g. in the case for typedefs of basic types
 * @param[in] die The type to get the underlying size for
 * @param[out] die_mem Where to store the die for the underlying type
 * @return Returns true if the underlying type with the size has been found
 */
static bool get_underlying_type_size (Dwarf_Die *const die, Dwarf_Die *die_mem)
{
    Dwarf_Attribute type_attr;
    bool got_underlying_type = false;
    bool more = true;

    *die_mem = *die;
    do
    {
        more = false;
        if (dwarf_hasattr (die_mem, DW_AT_byte_size) || (dwarf_tag (die_mem) == DW_TAG_array_type))
        {
            got_underlying_type = true;
        }
        else if (dwarf_attr (die_mem, DW_AT_type, &type_attr) != NULL)
        {
            if (dwarf_formref_die (&type_attr, die_mem) != NULL)
            {
                more = true;
            }
        }
    } while (more && (!got_underlying_type));

    return got_underlying_type;
}


/**
 * @todo Doesn't work with structures which containing anonymous unions such as PLX_MODE_PROP
 */
void display_structure (Dwarf_Die *const cudie, Dwarf_Die *const struct_die)
{
    int line;
    int column;
    Dwarf_Attribute attr;
    Dwarf_Die top_level_die;
    Dwarf_Die child_die;
    Dwarf_Die member_die;
    int top_level_tag;

    /*@todo decl_file was NULL for some output from aarch cross-compiled program.
     * Due to being built-ins?
     */
    const char *const decl_file = dwarf_decl_file (struct_die);
    if (decl_file != NULL)
    {
        /* Display the source location at which the structure is defined */
        (void) dwarf_decl_line (struct_die, &line);
        (void) dwarf_decl_column (struct_die, &column);
        printf ("Structure defined at %s line %d column %d\n", decl_file, line, column);

        /* Display the enum tag, if present.
         * Unlike the typedef, the enum tag is present in the debug information even if it isn't referenced in the code. */
        if (dwarf_attr (struct_die, DW_AT_name, &attr) != NULL)
        {
            printf ("  struct name=%s\n", dwarf_formstring (&attr));
        }

        /* Display the type, if present.
         * @todo doesn't seem present in DW_TAG_structure_type */
        if (dwarf_attr (struct_die, DW_AT_type, &attr) != NULL)
        {
            Dwarf_Die die_mem;
            Dwarf_Attribute base_attr;
            if (dwarf_formref_die (&attr, &die_mem) != NULL)
            {
                if (dwarf_attr (struct_die, DW_AT_name, &base_attr) != NULL)
                {
                    printf ("  type=%s\n", dwarf_formstring (&base_attr));
                }
            }
        }

        /* Display the size, if present */
        //if (get_underlying_type_size (struct_die, &child_die))
        if (dwarf_peel_type (struct_die, &child_die) == 0)
        {
            printf ("  size=%d\n", dwarf_bytesize (&child_die));
        }

        /* Display the typedefs, if any exist.
         * This searches for all typedefs in the same CU for a reference to the struct_die. */
        if (dwarf_child (cudie, &top_level_die) == 0)
        {
            do
            {
                top_level_tag = dwarf_tag (&top_level_die);
                if (top_level_tag == DW_TAG_typedef)
                {
                    if (dwarf_attr (&top_level_die, DW_AT_type, &attr) != NULL)
                    {
                        Dwarf_Die die_mem;

                        if (dwarf_formref_die (&attr, &die_mem) != NULL)
                        {
                            if (die_mem.addr == struct_die->addr)
                            {
                                (void) dwarf_attr (&top_level_die, DW_AT_name, &attr);
                                printf ("  DW_TAG_typedef name=%s\n", dwarf_formstring (&attr));
                            }
                        }
                    }
                }
            } while (dwarf_siblingof (&top_level_die, &top_level_die) == 0);
        }

        /* Display the name, size and offset of each structure member. */
        if (dwarf_child (struct_die, &member_die) == 0)
        {
            do
            {
                top_level_tag = dwarf_tag (&member_die);
                if (top_level_tag == DW_TAG_member)
                {
                    (void) dwarf_attr (&member_die, DW_AT_name, &attr);
                    printf ("  member %s", dwarf_formstring (&attr));
                    if (get_underlying_type_size (&member_die, &child_die))
                    {
                        if (dwarf_tag (&child_die) == DW_TAG_array_type)
                        {
                            /* Display the total size of a member which is an array */
                            Dwarf_Word size;

                            dwarf_aggregate_size (&child_die, &size);
                            printf (" size=%" PRIuMAX, size);
                        }
                        else
                        {
                            /* Display the size of a member which is a scalar */
                            printf (" size=%d", dwarf_bytesize (&child_die));
                        }
                    }
                    if (dwarf_attr (&member_die, DW_AT_data_member_location, &attr) != NULL)
                    {
                        Dwarf_Word value;
                        dwarf_formudata (&attr, &value);
                        printf (" offset=%" PRIuMAX, value);
                    }
                    printf ("\n");
                }
            } while (dwarf_siblingof (&member_die, &member_die) == 0);
        }
    }
}


int main (int argc, char *argv[])
{
    int exe_fd;
    int last_errno;

    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s <exe_with_debug_info>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    /* Open the executable file */
    char *exe_filename = argv[1];
    exe_fd = open (exe_filename, O_RDONLY);
    if (exe_fd == -1)
    {
        fprintf (stderr, "Failed to open %s", exe_filename);
        exit (EXIT_FAILURE);
    }

    /* Open handle to read DWARF information */
    Dwarf *const dbg = dwarf_begin (exe_fd, DWARF_C_READ);
    if (dbg == NULL)
    {
        last_errno = dwarf_errno ();
        fprintf (stderr, "dwarf_begin (%s) failed with errno %d\n%s\n", exe_filename, last_errno, dwarf_errmsg (last_errno));
        exit (EXIT_FAILURE);
    }

    /* Iterate through all compilation units */
    Dwarf_Off cu_off = 0;
    Dwarf_Off next_off;
    size_t header_size;
    Dwarf_Die cudie;
    Dwarf_Die top_level_die;
    int top_level_tag;
    while (!dwarf_nextcu (dbg, cu_off, &next_off, &header_size, NULL, NULL, NULL))
    {
        if (dwarf_offdie (dbg, cu_off + header_size, &cudie) == NULL)
        {
            break;
        }

        /* Iterate though all top-level children in the compilation unit */
        if (dwarf_child (&cudie, &top_level_die) == 0)
        {
            do
            {
                top_level_tag = dwarf_tag (&top_level_die);
                if ((top_level_tag == DW_TAG_structure_type) || (top_level_tag == DW_TAG_typedef))
                {
                    display_structure (&cudie, &top_level_die);
                }
            } while (dwarf_siblingof (&top_level_die, &top_level_die) == 0);
        }

        cu_off = next_off;
    }

    (void) dwarf_end (dbg);
    (void) close (exe_fd);

    return EXIT_SUCCESS;
}
