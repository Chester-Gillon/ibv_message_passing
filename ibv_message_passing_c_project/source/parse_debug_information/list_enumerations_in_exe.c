/*
 * @file list_enumerations_in_exe.c
 * @date 22 Sep 2019
 * @author Chester Gillon
 * @brief Program which uses libdw to demonstrate extracting the enumerations from the debug information from an ELF executable.
 * @todo For simplicity, the return value from all libdw functions is not always checked.
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
 * @brief Display the contents of one DW_TAG_enumeration_type read from the DWARF debugging information
 * @cudie[in] cudie The DIE for the compilation unit in which the enum_die was found.
 *                  Used to search for any typedef references.
 * @param[in] enum_die The DW_TAG_enumeration_type to display.
 */
static void display_enumeration (Dwarf_Die *const cudie, Dwarf_Die *const enum_die)
{
    int line;
    int column;
    Dwarf_Attribute attr;
    Dwarf_Die enum_entry;
    Dwarf_Die top_level_die;
    int top_level_tag;

    /* Display the source location at which the enumeration is defined */
    (void) dwarf_decl_line (enum_die, &line);
    (void) dwarf_decl_column (enum_die, &column);
    printf ("Enumeration defined at %s line %d column %d\n", dwarf_decl_file (enum_die), line, column);

    /* Display the enum tag, if present. */
    if (dwarf_attr (enum_die, DW_AT_name, &attr) != NULL)
    {
        printf ("  enum name=%s\n", dwarf_formstring (&attr));
    }

    /* Display the type, if present. */
    if (dwarf_attr (enum_die, DW_AT_type, &attr) != NULL)
    {
        Dwarf_Die die_mem;
        Dwarf_Attribute base_attr;
        if (dwarf_formref_die (&attr, &die_mem) != NULL)
        {
            dwarf_attr (&die_mem, DW_AT_name, &base_attr);
            printf ("  type=%s\n", dwarf_formstring (&base_attr));
        }
    }

    /* Display the typedef(s) for the enumeration, if any exist.
     * This searches for all typedefs in the same CU for a reference to the enum_die.
     * If there is a typedef for an enumeration, but the typedef isn't referenced in the code then the typedef
     * isn't present in the debug information. */
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
                        if (die_mem.addr == enum_die->addr)
                        {
                            (void) dwarf_attr (&top_level_die, DW_AT_name, &attr);
                            printf ("  DW_TAG_typedef name=%s\n", dwarf_formstring (&attr));
                        }
                    }
                }
            }
        } while (dwarf_siblingof (&top_level_die, &top_level_die) == 0);
    }

    /* Display all enumeration names and values */
    if (dwarf_child (enum_die, &enum_entry) == 0)
    {
        do
        {
            if (dwarf_tag (&enum_entry) == DW_TAG_enumerator)
            {
                Dwarf_Attribute value_attr;
                (void) dwarf_attr (&enum_entry, DW_AT_name, &attr);
                (void) dwarf_attr (&enum_entry, DW_AT_const_value, &value_attr);
                printf ("    DW_TAG_enumerator name=\"%s\" value=", dwarf_formstring (&attr));
                if (value_attr.form == DW_FORM_sdata)
                {
                    Dwarf_Sword value;

                    (void) dwarf_formsdata (&value_attr, &value);
                    printf ("%" PRIdMAX "\n", value);
                }
                else
                {
                    Dwarf_Word value;

                    (void) dwarf_formudata (&value_attr, &value);
                    printf ("%" PRIuMAX "\n", value);
                }
            }
        } while (dwarf_siblingof (&enum_entry, &enum_entry) == 0);
    }

    printf ("\n");
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
                if (top_level_tag == DW_TAG_enumeration_type)
                {
                    display_enumeration (&cudie, &top_level_die);
                }
            } while (dwarf_siblingof (&top_level_die, &top_level_die) == 0);
        }

        cu_off = next_off;
    }

    (void) dwarf_end (dbg);
    (void) close (exe_fd);

    return EXIT_SUCCESS;
}
