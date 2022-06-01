/*
 * @file clear_gcno_artificial_function_flags.c
 * @date 29 May 2022
 * @author Chester Gillon
 * @brief Read a gcno coverage "notes" file created by gcc, and clear the artificial flag for any functions.
 * @details Starting with gcc 8 the gcno file has an "artificial" flag for each function.
 *          When set the "artificial" flag is intended to indicate:
 *            "... the function is artificial and does not exist in a source file".
 *
 *          As a result the gcc gcov and lcov programs used to generate a coverage report discard any coverage results
 *          for functions flagged as "artificial".
 *
 *          Think the "artificial" flag was added to avoid "useless output" being reported for C++ programs.
 *          However, with GNAT based upon gcc 8.3.1, 9.3.1 or 10.3.1 it was found functions used to implement Ada tasks
 *          are getting flagged as "artificial" which resulted in no coverage being reported for Ada tasks.
 *
 *          This program was created as a work-around to allow coverage to be collected for Ada tasks, by acting a step
 *          between the compilation which creates the gcno files, and the generation of coverage reports which reads
 *          the gcno files. For any function in the gcno file which has the artificial flag set true, the gcno file is
 *          modified to clear the flag provided the referenced source file exists (to guard against the coverage report
 *          trying to use a source file which doesn't exist).
 *
 *          The format of the gcno file can change between major compiler versions, and this program reads the generating
 *          compiler version from the header of the gcno file to determine which version specific fields to expect.
 *          This program only needs to interpret the file header and function tag, any other tags are skipped based upon
 *          the record length field.
 *
 *          At the time of development this program was tested with the gcno files created by GNAT based upon gcc versions
 *          6.3.1, 7.3.1, 8.3.1, 9.3.1 and 10.3.1. Based upon a check of the gcc source code expect it should operate
 *          correctly on gcno files produced by gcc 6 to gcc 11.
 *
 *          If used on a gcno file with a header or function tag with different fields to that the program expects, it
 *          should report an error due to not following the length of each tag correctly and therefore not reaching
 *          the end of the file.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/* File magic */
#define GCOV_NOTE_MAGIC (0x67636e6f) /* "gcno" */

/* Compiler versions at the limits at which the gcno file format has been investigated, or when the file format
 * used by this program has changed. */
/* Specific compiler versions compared in this program */
#define GCOV_VERSION_6_3_0  0x60300
#define GCOV_VERSION_8_0_0  0x80000
#define GCOV_VERSION_9_0_0  0x90000
#define GCOV_VERSION_11_3_0 0xb0300

/* The min and max GCC versions over which the gcno file format was checked when writing this program in terms of
 * a. The file header structure
 * b. The tag function structure
 *
 * This program may not operate correctly with gcno files produced by the gcc compilers outside of this range of versions
 */
#define GCOV_MIN_SUPPORTED_VERSION GCOV_VERSION_6_3_0
#define GCOV_MAX_SUPPORTED_VERSION GCOV_VERSION_11_3_0

/* The tags this program reads */
#define GCOV_TAG_FUNCTION 0x01000000

/* The name of the gcno file being read, for reporting in any errors */
static char gcno_filename[PATH_MAX];

/* The gcno file which is being read */
static FILE *gcno_file;

/* Set true have modified the gcno_file contents, to report a warning if later get a file error */
static bool gcno_modified;

/* The stat() information for the gcno_filename, containing the size of the file used when validating length fields don't cause
 * an attempt to read off the end of the file. */
static struct stat gcno_stat;

/* Used to dynamically allocate a buffer for a string read from a gcno file */
typedef struct
{
    /* The null terminated string which has been read */
    char *string;
    /* The current length of the buffer for string, used to re-size the buffer as required */
    size_t allocated_length_bytes;
} gcno_string_t;


/**
 * @brief Exit the program after an error reading the gcno file, reporting a warning if have modified the file contents.
 */
static void exit_after_file_error (void)
{
    if (gcno_modified)
    {
        printf ("Warning: File error occurred after modification, file may be corrupted\n");
    }
    exit (EXIT_FAILURE);
}


/**
 * @brief Get the current offset in the gcno file
 */
static long int get_current_gcno_offset (void)
{
    long int current_offset = ftell (gcno_file);

    if (current_offset < 0)
    {
        printf ("ftell() failed for %s\n", gcno_filename);
        exit_after_file_error ();
    }

    return current_offset;
}


/**
 * @brief Set the current offset in the gcno file.
 * @details Allow either values in the gcno file to be re-written, or to skip tags this program doesn't process.
 * @param[in] new_offset The offset from the start of the file to set the position to.
 */
static void set_current_gcno_offset (const long int new_offset)
{
    int rc = fseek (gcno_file, new_offset, SEEK_SET);

    if (rc != 0)
    {
        printf ("Failed to seek to offset %ld in %s\n", new_offset, gcno_filename);
        exit_after_file_error ();
    }
}


/**
 * @brief Read one unsigned 32-bit word from a gcno file
 * @param[in] field_description The description of what are trying to read, for reporting in any error
 * @return Returns the 32-bit word read from the gcno file
 */
static uint32_t gcno_read_unsigned (const char *const field_description)
{
    uint32_t value;
    const uint32_t num_read = fread (&value, sizeof (uint32_t), 1, gcno_file);

    if (num_read != 1)
    {
        printf ("Unexpected eof in %s trying to read \"%s\"\n", gcno_filename, field_description);
        exit_after_file_error ();
    }

    return value;
}


/**
 * @brief Read a string from a gcno file
 * @param[in/out] string The buffer for the string read. in/out as expands the buffer as required.
 */
static void gcno_read_string (gcno_string_t *const string)
{
    /* Read the length of the string */
    const uint32_t string_length_words = gcno_read_unsigned ("string length");
    const uint32_t string_length_bytes = string_length_words * sizeof (uint32_t);

    /* Check that the string length will not cause an attempt to read off the end of the gcno file, which is a sign that
     * either this program isn't reading the file format correctly or the gcno file is corrupt. */
    const long int string_start_offset = get_current_gcno_offset ();
    if ((string_start_offset + string_length_bytes) > gcno_stat.st_size)
    {
        printf ("string with length %u (data words) starting at offset %ld extends beyond the end of file %s of size %ld bytes\n",
                string_length_words, string_start_offset, gcno_filename, gcno_stat.st_size);
        exit_after_file_error ();
    }

    /* Allocate a buffer to store the string, growing the buffer as required.
     * If a non-zero length is specified then the string should contain null padding.
     * If a zero-length is specified then just ensure the buffer contains a single byte for padding.
     */
    const size_t required_length_bytes = (string_length_bytes > 0) ? string_length_bytes : 1;
    if (required_length_bytes > string->allocated_length_bytes)
    {
        string->string = realloc (string->string, required_length_bytes);
        if (string->string == NULL)
        {
            printf ("Unable to allocate %zu bytes for string\n", required_length_bytes);
            exit_after_file_error ();
        }
        string->allocated_length_bytes = required_length_bytes;
    }

    if (string_length_bytes > 0)
    {
        /* Read the string from the gcno file */
        const uint32_t bytes_read = fread (string->string, 1, string_length_bytes, gcno_file);
        if (bytes_read != string_length_bytes)
        {
            printf ("Unexpected eof in %s string to read string of length %u (words)\n", gcno_filename, string_length_words);
            exit_after_file_error ();
        }

        /* Verify that the string in the gcno file is null terminated */
        if (strnlen (string->string, string_length_bytes) == string_length_bytes)
        {
            printf ("String of length %u (words) read from %s at offset %lu is not null terminated\n",
                    string_length_words, gcno_filename, string_start_offset);
            exit_after_file_error ();
        }
    }
    else
    {
        /* An empty string in the gcno file */
        string->string[0] = '\0';
    }
}


/**
 * @brief Read the gcc compiler version which created a gcno file
 * @return The compiler major and minor version encoded as one byte per version field,
 *         which allows versions to be compared numerically.
 *         E.g.:
 *         - gcc  6.3.x is  0x60300
 *         - gcc 10.3.x is  0xa0300
 *
 *         The micro-version isn't encoded, so the least significant byte is zero.
 */
static uint32_t gcno_read_compiler_version (void)
{
    const uint32_t raw_version = gcno_read_unsigned ("compiler version");
    const uint32_t a =  raw_version >> 24;
    const uint32_t b = (raw_version >> 16) & 0xff;
    const uint32_t c = (raw_version >>  8) & 0xff;
    uint32_t major;
    uint32_t minor;

    if (a < 'A')
    {
        /* Old style version number with single character major and two character minor */
        major = a - '0';
        minor = ((b - '0') * 10) + (c - '0');
    }
    else
    {
        /* New style version number with two character major and single character minor.
         * Identified when first character starts from 'A'. */
        major = ((a - 'A') * 10) + (b - '0');
        minor = c - '0';
    }

    return (major << 16) | (minor << 8);
}


/**
 * @brief Print a compiler version in human readable form, containing the major and minor versions.
 * @param[in] compiler_version Compiler version in the format returned by gcno_read_compiler_version()
 */
static void print_compiler_version (const uint32_t compiler_version)
{
    const uint32_t major = (compiler_version >> 16) & 0xff;
    const uint32_t minor = (compiler_version >>  8) & 0xff;

    printf ("%u.%u.x", major, minor);
}


/**
 * @brief Read and process a function tag from a gcno file
 * @details This provides the functionality of this program, which if finds a function with the artificial flag set then
 *          attempts to modify the gcno file to clear the flag (provided the referenced source file exists).
 * @param[in] compiler_version Used to identify the compiler version specific fields to read from the function tag.
 */
static void process_function_tag (const uint32_t compiler_version)
{
    /* static so can re-use the memory store between calls */
    static gcno_string_t function_name = {0};
    static gcno_string_t source_filename = {0};
    char location[40] = {0};

    /* Read the contents of the function tag, with the presence of some fields being compiler version specific */
    const bool has_artificial_flag = compiler_version >= GCOV_VERSION_8_0_0;
    long int artificial_offset = -1;
    bool artificial = false;
    (void) gcno_read_unsigned ("ident");
    (void) gcno_read_unsigned ("lineno_checksum");
    (void) gcno_read_unsigned ("cfg_checksum");
    gcno_read_string (&function_name);
    if (has_artificial_flag)
    {
        artificial_offset = get_current_gcno_offset ();
        artificial = gcno_read_unsigned ("artificial") != 0;
    }
    gcno_read_string (&source_filename);
    if (compiler_version < GCOV_VERSION_8_0_0)
    {
        const uint32_t lineno = gcno_read_unsigned ("lineno");
        snprintf (location, sizeof (location), ":%u", lineno);
    }
    else
    {
        const uint32_t line_start = gcno_read_unsigned ("line_start");
        const uint32_t column_start = gcno_read_unsigned ("column_start");
        const uint32_t line_end = gcno_read_unsigned ("line_end");

        if (compiler_version >= GCOV_VERSION_9_0_0)
        {
            const uint32_t column_end = gcno_read_unsigned ("column_end");
            snprintf (location, sizeof (location), ":%u:%u-%u:%u", line_start, column_start,
                      line_end, column_end);
        }
        else
        {
            snprintf (location, sizeof (location), ":%u:%u:%u", line_start, column_start, line_end);
        }
    }

    /* If the function tag has the artificial flag, check if the flag needs to be cleared */
    if (has_artificial_flag)
    {
        if (artificial)
        {
            struct stat source_stat;
            int rc;

            rc = stat (source_filename.string, &source_stat);
            if (rc == 0)
            {
                /* The artificial flag is set for a function for which the referenced source file exists.
                 * Re-write the artificial flag as clear so the lcov tools should then be able to report coverage information
                 * for the function. After seeking backwards to re-write the artificial flag, leaves the current gcno file
                 * offset at end of the function tag ready to read the following tag in the gcno file.
                 *
                 * The location of the function in the source file is reported to aid understanding which part of the source
                 * code the "artificial" function relates to. */
                const uint32_t cleared_artificial_flag = 0;
                const long int tag_end_offset = get_current_gcno_offset ();

                set_current_gcno_offset (artificial_offset);
                gcno_modified = true;
                const size_t num_written = fwrite (&cleared_artificial_flag, sizeof (cleared_artificial_flag), 1, gcno_file);
                if (num_written != 1)
                {
                    printf ("Failed to re-write artificial field in %s\n", gcno_filename);
                    exit_after_file_error ();
                }
                set_current_gcno_offset (tag_end_offset);
                printf ("Cleared artificial flag in %s for function %s at source file %s%s\n",
                        gcno_filename, function_name.string, source_filename.string, location);
            }
            else
            {
                /* genhtml can report errors if a source file can't be found.
                 * Therefore, don't clear the artificial flag if the referenced source file doesn't exist. */
                printf ("Leaving artificial flag set for function %s in gcno file %s since the source file %s doesn't exist\n",
                        function_name.string, gcno_filename, source_filename.string);
            }
        }
    }
}


int main (int argc, char *argv[])
{
    gcno_string_t cwd = {0};
    int rc;

    if (argc != 2)
    {
        printf ("Usage: %s <gcno_file>\n", argv[0]);
        exit (EXIT_FAILURE);
    }
    snprintf (gcno_filename, sizeof (gcno_filename), "%s", argv[1]);

    /* Get size of the gcno file so can detect end of file after seeking (since seeking clears the eof flag) */
    rc = stat (gcno_filename, &gcno_stat);
    if (rc != 0)
    {
        printf ("Failed to stat %s\n", gcno_filename);
        exit (EXIT_FAILURE);
    }

    /* All length fields in the gcno file count 32-bit words */
    if ((gcno_stat.st_size % sizeof (uint32_t)) != 0)
    {
        printf ("%s size is not a multiple of 32-bit words\n", gcno_filename);
        exit (EXIT_FAILURE);
    }

    /* Open the file for modification. Modification will only occur if see the artificial flag set for a function */
    gcno_file = fopen (gcno_filename, "r+b");
    if (gcno_file == NULL)
    {
        printf ("Unable to open %s\n", gcno_filename);
        exit (EXIT_FAILURE);
    }
    gcno_modified = false;

    /* Read magic to check that is a gcno file */
    const uint32_t gcno_magic = gcno_read_unsigned ("gcno magic");
    if (gcno_magic != GCOV_NOTE_MAGIC)
    {
        printf ("%s is not a native (little-endian) gcno file\n", gcno_filename);
        exit (EXIT_FAILURE);
    }

    /* Read version of compiler which created the gcno file, so can allow for differences in file format */
    const uint32_t compiler_version = gcno_read_compiler_version ();

    /* Produce a warning if the program may not correctly process the gcno file format, based upon the version of gcc
     * which created the file. */
    if ((compiler_version < GCOV_MIN_SUPPORTED_VERSION) || (compiler_version > GCOV_MAX_SUPPORTED_VERSION))
    {
        printf ("Warning: %s was created by gcc ", gcno_filename);
        print_compiler_version (compiler_version);
        printf (". Whereas this program was designed to operate with the gcno file formats between gcc ");
        print_compiler_version (GCOV_MIN_SUPPORTED_VERSION);
        printf (" to ");
        print_compiler_version (GCOV_MAX_SUPPORTED_VERSION);
        printf ("\n");
    }

    /* Skip remaining file header fields */
    (void) gcno_read_unsigned ("file timestamp");
    if (compiler_version >= GCOV_VERSION_9_0_0)
    {
        gcno_read_string (&cwd);
    }
    if (compiler_version >= GCOV_VERSION_8_0_0)
    {
        (void) gcno_read_unsigned ("support_unexecuted_blocks");
    }

    /* Process tags in the gcno file. This program is only interested in the tag function; other tags are skipped. */
    long int next_tag_offset = get_current_gcno_offset ();
    while (next_tag_offset != gcno_stat.st_size)
    {
        /* Read tag header */
        const uint32_t tag = gcno_read_unsigned ("tag");
        const uint32_t tag_length = gcno_read_unsigned ("tag length");

        /* Check that the tag length will not cause an attempt to read off the end of the gcno file, which is a sign that
         * either this program isn't reading the file format correctly or the gcno file is corrupt. */
        const long int tag_data_start_offset = get_current_gcno_offset ();
        next_tag_offset = tag_data_start_offset + ((long int) tag_length * sizeof (uint32_t));
        if (next_tag_offset > gcno_stat.st_size)
        {
            printf ("tag 0x%x with length %u (data words) starting at offset %ld extends beyond the end of file %s of size %ld bytes\n",
                    tag, tag_length, tag_data_start_offset, gcno_filename, gcno_stat.st_size);
            exit_after_file_error ();
        }

        if (tag == GCOV_TAG_FUNCTION)
        {
            /* Process the function tag. The tag is processed even if the artificial flag isn't present for the compiler version
             * to check that still read the expected amount of data for tag, as part of checking are parsing the file format
             * correctly. */
            process_function_tag (compiler_version);

            const long int bytes_read_from_tag = get_current_gcno_offset () - tag_data_start_offset;
            const long int words_read_from_tag = bytes_read_from_tag / sizeof (uint32_t);
            if (words_read_from_tag != tag_length)
            {
                printf ("Only read %ld words from function tag start at offset %ld, not the %u words specified in the length in %s\n",
                        words_read_from_tag, tag_data_start_offset, tag_length, gcno_filename);
                exit_after_file_error ();
            }
        }
        else
        {
            /* Skip over the data for a type of tag this program doesn't process */
            set_current_gcno_offset (next_tag_offset);
        }
    }

    rc = fclose (gcno_file);
    if (rc != 0)
    {
        printf ("Error closing %s\n", gcno_filename);
        exit_after_file_error ();
    }

    return EXIT_SUCCESS;
}
