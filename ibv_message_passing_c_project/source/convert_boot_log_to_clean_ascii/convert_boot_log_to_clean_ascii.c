/*
 * @file convert_boot_log_to_clean_ascii.c
 * @date 15 Jan 2023
 * @author Chester Gillon
 * @details Program which converts a Linux boot.log into "clean" ASCII by removing
 *          a. ANSI escape sequences, which are either for colouring the text or cursor movement
 *          b. Carriage Returns
 */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


/**
 * @brief Determine if the start of text string matches an ANSI CSI (Control Sequence Introducer) sequence
 * @details The sequence to match was taken from:
 *             https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
 * @param[in] text The text to match against an ANSI CSI
 * @param[out] csi_len The number of bytes in the matched ANSI CSI
 * @return Returns true if the start of text string matches an ANSI CSI
 */
static bool ansi_csi_match (const char *const text, size_t *const csi_len)
{
    const char *const ansi_csi_start = "\e[";
    bool matches_ansi_csi = false;

    *csi_len = 0;
    if (strncmp (text, ansi_csi_start, strlen (ansi_csi_start)) == 0)
    {
        /* Skip start of the CSI */
        (*csi_len) += strlen (ansi_csi_start);

        /* Skip parameter bytes */
        while ((text[*csi_len] >= 0x30) && (text[*csi_len] <= 0x3f))
        {
            (*csi_len)++;
        }

        /* Skip intermediate bytes */
        while ((text[*csi_len] >= 0x20) && (text[*csi_len] <= 0x2f))
        {
            (*csi_len)++;
        }

        /* Check the sequence ends with a "final byte" */
        if ((text[*csi_len] >= 0x40) && (text[*csi_len] <= 0x7e))
        {
            (*csi_len)++;
            matches_ansi_csi = true;
        }
    }

    return matches_ansi_csi;
}

int main (int argc, char *argv[])
{
    int rc;
    FILE *log_file;

    const char *const switch_to_utf8 = "\e%G";
    const char *const erase_line = "\e[K";

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
    size_t file_pos = 0;
    size_t ansi_csi_len = 0;
    while (file_pos < stat_buf.st_size)
    {
        if (strncmp (&log_file_contents[file_pos], switch_to_utf8, strlen (switch_to_utf8)) == 0)
        {
            /* Skip the escape sequence to switch to UFT-8, which CentOS 6 can output */
            file_pos += strlen (switch_to_utf8);
        }
        else if (ansi_csi_match (&log_file_contents[file_pos], &ansi_csi_len))
        {
            if ((ansi_csi_len == strlen (erase_line)) &&
                (strncmp (&log_file_contents[file_pos], erase_line, strlen (erase_line)) == 0))
            {
                /* If the ANSI escape sequence is an erase line, insert a new line in the output
                 * as the erase line is used is used to report progress for start jobs. */
                printf ("\n");
            }

            /* Skip over the ANSI escape sequence */
            file_pos += ansi_csi_len;
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

    free (log_file_contents);

    return EXIT_SUCCESS;
}
