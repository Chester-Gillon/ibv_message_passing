/*
 * @file convert_boot_log_to_clean_ascii.c
 * @date 15 Jan 2023
 * @author Chester Gillon
 * @details Program which converts a Linux boot.log into "clean" ASCII by removing
 *          a. ANSI escape sequences, which are either for colouring the text or cursor movement
 *          b. Carriage Returns
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <sys/types.h>
#include <regex.h>
#include <sys/stat.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
    int rc;
    regex_t ansi_reg;
    char errbuf[1024];
    FILE *log_file;
    regmatch_t match;

    const char *const erase_line = "\e[K";

    /* Compile a regex for an ANSI Control Sequence Introducer */
    const char *const regex_expression = "\e\\[[0-9;]*[a-zA-Z]";
    rc = regcomp (&ansi_reg, regex_expression, REG_EXTENDED);
    if (rc != 0)
    {
        regerror (rc, &ansi_reg, errbuf, sizeof (errbuf));
        printf ("regcomp() failed : %s\n", errbuf);
        exit (EXIT_FAILURE);
    }

    if (argc == 1)
    {
        printf ("Usage: %s <boot.log>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    /* Read the entire log file into memory for processing, to allow for arbitrary length lines */
    const char *const log_filename = argv[1];
    struct stat stat_buf;
    rc = stat (log_filename, &stat_buf);
    if (rc != 0)
    {
        printf ("Failed to stat %s\n", log_filename);
        exit (EXIT_FAILURE);
    }
    char *log_file_contents = calloc (stat_buf.st_size + 1, 1);
    if (log_file_contents == NULL)
    {
        printf ("Failed to allocate memory to read %s\n", log_filename);
    }

    log_file = fopen (log_filename, "rb");
    if (log_file == NULL)
    {
        printf ("Failed to open %s\n", log_filename);
        exit (EXIT_FAILURE);
    }

    ssize_t num_read;
    num_read = fread (log_file_contents, 1, stat_buf.st_size, log_file);
    if (num_read != stat_buf.st_size)
    {
        printf ("Failed to read %s\n", log_filename);
        exit (EXIT_FAILURE);
    }
    (void) fclose (log_file);

    /* Process the log file, writing its cleaned contents to standard output */
    off_t file_pos = 0;
    while (file_pos < stat_buf.st_size)
    {
        rc = regexec (&ansi_reg, &log_file_contents[file_pos], 1, &match, 0);
        if (rc == 0)
        {
            /* Output characters up until the start of the ANSI escape sequence */
            for (regoff_t start_offset = 0; start_offset < match.rm_so; start_offset++)
            {
                if (log_file_contents[file_pos + start_offset] != '\r')
                {
                    printf ("%c", log_file_contents[file_pos + start_offset]);
                }
            }

            if (strncmp (&log_file_contents[file_pos + match.rm_so], erase_line, strlen (erase_line)) == 0)
            {
                /* If the ANSI escape sequence is an erase line, insert a new line in the output
                 * as the erase line is used is used to report progress for start jobs. */
                printf ("\n");
            }

            /* Skip over the ANSI escape sequence */
            file_pos += match.rm_eo;
        }
        else if (log_file_contents[file_pos] == '\r')
        {
            /* Skip carriage return */
            file_pos++;
        }
        else
        {
            /* Copy log file character to output */
            printf ("%c", log_file_contents[file_pos]);
            file_pos++;
        }
    }

    regfree (&ansi_reg);
    free (log_file_contents);

    return EXIT_SUCCESS;
}
