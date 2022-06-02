/*
 * @file compare_ada_files.cpp
 * @date 21 May 2022
 * @author Chester Gillon
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <set>
#include <string>

#include <limits.h>
#include <ftw.h>


/* Used to contain the relative paths of all source files from one directory root to be compared */
typedef std::set<std::string> source_file_list_t;


/* Maximum number of open file descriptors used by ntfw().
 * Conservative value recommended by https://stackoverflow.com/questions/8436841/how-to-recursively-list-directories-in-c-on-linux
 *
 * sysconf() isn't implemented in mingw, so haven't used the other suggestion of basing upon sysconf(_SC_OPEN_MAX)
 */
#define MAX_NFTW_OPEN_DESCRIPTORS 15

/* Contains one source tree */
typedef struct
{
    /* The root directory for the source tree, which ends with a directory separator */
    char root_directory[PATH_MAX];
    /* The paths to the Ada source file in the tree, relative to the root directory */
    source_file_list_t source_list;
} source_tree_t;

/* The two directories trees to be compared */
static source_tree_t left_tree;
static source_tree_t right_tree;

/* The source tree currently being populated by nftw() */
static source_tree_t *current_tree = NULL;

/* The length of the source tree root currently being populated by nftw().
 * Used so that current_source_list is only populated with the pathname components under the root,
 * so will be the same for both trees being compared. */
static size_t current_source_tree_root_prefix_len;

/* This list of extensions considered as Ada source files */
static const char *const ada_source_file_extensions[] =
{
    ".ada",
    ".adb",
    ".ads"
};
static const int num_ada_source_file_extensions = sizeof (ada_source_file_extensions) / sizeof (ada_source_file_extensions[0]);

/* The possible comparison results between two directory trees */
typedef enum
{
    /* The file only appears in the left source tree */
    FILE_COMPARISON_LEFT_ONLY,
    /* The file only appears in the right source tree */
    FILE_COMPARISON_RIGHT_ONLY,
    /* The file is binary equal between the left and right source tree */
    FILE_COMPARISON_BINARY_EQUAL,
    /* The file is lexically different between the left and right source tree, meaning some functional difference in the
     * Ada statements */
    FILE_COMPARISON_DIFFERENT,
    /* The file is lexically equal between the left and right source tree, meaning differences in either:
     * a. Comments
     * b. Whitespace
     * c. Casing
     * d. Wrapping of statements across source lines
     */
    FILE_COMPARISON_LEXICAL_EQUAL,

    FILE_COMPARISON_ARRAY_SIZE
} file_comparison_t;

static const char *const file_comparison_names[FILE_COMPARISON_ARRAY_SIZE] =
{
    [FILE_COMPARISON_LEFT_ONLY    ] = "Left only",
    [FILE_COMPARISON_RIGHT_ONLY   ] = "Right only",
    [FILE_COMPARISON_BINARY_EQUAL ] = "Binary equal",
    [FILE_COMPARISON_DIFFERENT    ] = "Different",
    [FILE_COMPARISON_LEXICAL_EQUAL] = "Lexical equal"
};

/* Ada identifiers after which to insert a new line in the lexical contents, when the identifier is followed immediately
 * by another identifier. Done to try and get some meaningful line breaks in the lexical contents when manually comparing
 * the results of the comparison. */
static const char *const identifiers_for_line_breaks[] =
{
    "is", /* start of procedure, function, package, case */
    "begin",
    "declare",
    "loop",
    "then",
    "record",
    "do"
};
static const size_t num_identifiers_for_line_breaks =
        sizeof (identifiers_for_line_breaks) / sizeof (identifiers_for_line_breaks[0]);


/* Used to hold the contents of an Ada source file to be compared */
typedef struct
{
    /* The size of the file in bytes */
    size_t file_length;
    /* The raw byte contents of the file. */
    char *file_contents;
    /* The lexical contents of the file, processed such that:
     * a. Comments are removed.
     * b. Identifiers are converted to lower case to allow for Ada being case-insensitive for identifiers.
     * c. White space is removed except for a single characters between identifiers.
     * d. May contain a carriage return to break into lines at a "convenient" place when dumping the lexical text to
     *    a file for external comparison. */
    char *lexical_contents;
    /* The number of characters in the lexical_contents array. */
    size_t lexical_length;
    /* The size of the allocated lexical_contents array, used to dynamically grow the array */
    size_t lexical_allocated_size;
    /* When true the lexical contents parser is currently in a comment */
    bool in_comment;
    /* When true the lexical contents parser is currently in a character literal (string) */
    bool in_character_literal;
    /* When true the lexical contents parser is currently in a numeric literal */
    bool in_numeric_literal;
    /* When true the lexical contents parser is currently in an identifier */
    bool in_identifier;
    /* When true the previous lexical contents was an identifier */
    bool previous_lexical_contents_identifier;
    /* When previous_lexical_contents_identifier is true, gives the start index and length of the previous identifier
     * in the lexical contents */
    size_t previous_identifier_start_index;
    size_t previous_identifier_length;
    /* When true the previous lexical contents was a numeric literal */
    bool previous_lexical_contents_numeric_literal;
} compared_file_contents_t;


/**
 * @brief Callback function for nftw()
 * @details When passes an Ada source file inserts into current_source_list
 * @param[in] fpath The pathname in the directory tree
 * @oaram[in] sb Not used
 * @param[in] flagtype The type of path:
 *            a. Files are processed to check if an Ada source file
 *            b. Directories are ignored
 *            c. Errors accessing files cause the program to be aborted, to avoid reporting partial trees
 */
static int tree_walk_callback (const char *fpath, const struct stat *sb, int flagtype, struct FTW *ftwbuf)
{
    const char *const filename = &fpath[ftwbuf->base];

    /* On the first call at directory level zero work out how many characters to ignore from the start of fpath
     * so that only store the pathname components of each source file relative to the root. */
    if ((current_source_tree_root_prefix_len == 0) && (ftwbuf->level == 0))
    {
        current_source_tree_root_prefix_len = strlen (fpath);
        if (current_source_tree_root_prefix_len > 0)
        {
            const char last_char = fpath[current_source_tree_root_prefix_len - 1];

            if ((last_char != '/') && (last_char != '\\'))
            {
                /* Skip the assumed directory separator which will be present in subsequent calls */
                current_source_tree_root_prefix_len++;
            }
        }
    }

    switch (flagtype)
    {
    case FTW_F:
    case FTW_SL:
        /* If the file extension is that of an Ada source file, then store the filename in the source file list */
        {
            const char *const extension = strrchr (filename, '.');

            if (extension != NULL)
            {
                for (int extension_index = 0; extension_index < num_ada_source_file_extensions; extension_index++)
                {
                    if (strcasecmp (extension, ada_source_file_extensions[extension_index]) == 0)
                    {
                        if (current_tree->root_directory[0] == '\0')
                        {
                            /* Save the canonicalised root directory */
                            snprintf (current_tree->root_directory, sizeof (current_tree->root_directory), fpath);
                            current_tree->root_directory[current_source_tree_root_prefix_len] = '\0';
                        }
                        current_tree->source_list.insert (&fpath[current_source_tree_root_prefix_len]);
                        break;
                    }
                }
            }
        }
        break;

    case FTW_D:
    case FTW_DP:
        /* Nothing to do for a directory */
        break;

    case FTW_DNR:
        printf ("Error: %s is a directory which can't be read\n", fpath);
        exit (EXIT_FAILURE);
        break;

    case FTW_NS:
        printf ("Error: stat() call failed on %s\n", fpath);
        exit (EXIT_FAILURE);
        break;

    case FTW_SLN:
        printf ("Error: %s is a symbolic link pointing to a nonexistent file\n", fpath);
        exit (EXIT_FAILURE);
        break;
    }

    /* Continue the tree walk */
    return 0;
}


/**
 * @brief Append one character to the lexical contents for a file, growing the array as required.
 * @param[in/out] contents The file lexical contents to append to.
 * @param[in] ch The character to append.
 */
static void append_lexical_char (compared_file_contents_t *const contents, const char ch)
{
    if (contents->lexical_length == contents->lexical_allocated_size)
    {
        const size_t grow_size = 4096;
        contents->lexical_allocated_size += grow_size;
        contents->lexical_contents = (char *) realloc (contents->lexical_contents, contents->lexical_allocated_size);
        if (contents->lexical_contents == NULL)
        {
            printf ("Error: memory allocation failed in append_lexical_char()\n");
            exit (EXIT_FAILURE);
        }
    }

    contents->lexical_contents[contents->lexical_length] = ch;
    contents->lexical_length++;
}


/**
 * @brief Called when detect a new identifier or numeric literal to insert a white space character as a separator.
 * @details This is so that preserve whitespace which is lexically significant.
 *          It should also ensure that the resulting lexical_contents still compiles.
 * @param[in/out] contents The current lexical contents, to append a separating whitespace character when required.
 */
static void seperate_identifiers_or_numeric_literals (compared_file_contents_t *const contents)
{
    /* If the previous lexical contents was an identifier insert a white space character to separate the identifiers. */
    if (contents->previous_lexical_contents_identifier)
    {
        bool at_line_break = false;
        for (size_t identifier_index = 0;
             (!at_line_break) && (identifier_index < num_identifiers_for_line_breaks);
             identifier_index++)
        {
            const char *const compared_identifier = identifiers_for_line_breaks[identifier_index];

            if ((contents->previous_identifier_length == strlen (compared_identifier)) &&
                (strncmp (&contents->lexical_contents[contents->previous_identifier_start_index],
                          compared_identifier, contents->previous_identifier_length) == 0))
            {
                at_line_break = true;
            }
        }
        append_lexical_char (contents, at_line_break ? '\n' : ' ');
        contents->previous_lexical_contents_identifier = false;
    }
    else if (contents->previous_lexical_contents_numeric_literal)
    {
        append_lexical_char (contents, ' ');
        contents->previous_lexical_contents_numeric_literal = false;
    }
}


/**
 * @brief Parse the lexical contents of an input source file.
 * @param[in/out] contents On entry contains the raw file_contents[] to parse.
 *                         On exit the lexical_contents[] array has been populated with the lexical contents to compare.
 */
static void parse_lexical_file_contents (compared_file_contents_t *const contents)
{
    /* Initialise the state of the parser */
    contents->in_comment = false;
    contents->in_character_literal = false;
    contents->in_identifier = false;
    contents->previous_identifier_start_index = 0;
    contents->previous_identifier_length = 0;
    contents->previous_lexical_contents_identifier = false;
    contents->in_numeric_literal = false;
    contents->previous_lexical_contents_numeric_literal = false;

    /* Process all the characters in the raw input file */
    size_t file_index = 0;
    while (file_index < contents->file_length)
    {
        const char *ch = &contents->file_contents[file_index];
        size_t num_chars_consumed = 1;
        const size_t num_remaining_chars = contents->file_length - file_index;

        if (contents->in_comment)
        {
            /* Comment finishes at end of line */
            if (*ch == '\n')
            {
                contents->in_comment = false;
            }
        }
        else if (contents->in_character_literal)
        {
            if ((num_remaining_chars >= 2) && (ch[0] == '\"') && (ch[1] == '\"'))
            {
                /* Found an embedded quote in the character literal */
                append_lexical_char (contents, *ch++);
                append_lexical_char (contents, *ch++);
                num_chars_consumed++;
            }
            else if (contents->file_contents[file_index] == '\"')
            {
                /* Found the end of the character literal */
                append_lexical_char (contents, *ch);
                contents->in_character_literal = false;
            }
            else
            {
                /* One character inside a character literal */
                append_lexical_char (contents, *ch);
            }
        }
        else if (contents->in_identifier)
        {
            if (isalnum (*ch) || (*ch == '_'))
            {
                /* One character inside an identifier, store as lower case */
                append_lexical_char (contents, tolower (*ch));
                contents->previous_identifier_length++;
            }
            else
            {
                /* The identifier has been terminated.
                 * Don't consume the character which is re-evaluated by the next loop iteration.
                 * Flag that the previous lexical contents was an identifier, to allow a space to be inserted to
                 * separate two adjacent identifiers / numeric literals. */
                contents->in_identifier = false;
                contents->previous_lexical_contents_identifier = true;
                num_chars_consumed = 0;
            }
        }
        else if (contents->in_numeric_literal)
        {
            if (isxdigit (*ch) || (*ch == '_') || (*ch == '#') || (*ch == '.'))
            {
                /* One character inside an identifier, store as lower case in case a hex digit */
                append_lexical_char (contents, tolower (*ch));
            }
            else
            {
                /* The numeric literal has been terminated.
                 * Don't consume the character which is re-evaluated by the next loop iteration.
                 * Flag that the previous lexical contents was a numeric literal, to allow a space to be inserted to
                 * separate two adjacent identifiers / numeric literals. */
                contents->in_numeric_literal = false;
                contents->previous_lexical_contents_numeric_literal = true;
                num_chars_consumed = 0;
            }
        }
        else if ((num_remaining_chars >= 2) && (ch[0] == '-') && (ch[1] == '-'))
        {
            /* Found start of comment, which isn't part of the lexical contents */
            contents->in_comment = true;
            num_chars_consumed++;
        }
        else if (*ch == '\"')
        {
            /* Found start of character literal, which forms part of the lexical contents */
            contents->in_character_literal = true;
            contents->previous_lexical_contents_identifier = false;
            contents->previous_lexical_contents_numeric_literal = false;
            append_lexical_char (contents, *ch);
        }
        else if ((num_remaining_chars >= 3) && (ch[0] == '\'') && (ch[2] == '\''))
        {
            /* Store a character literal.
             * @todo This simplistic test gets the wrong answer in the case of:
             *          test : Character := Character'(' ');
             *
             *       Since the character literal is considered as open-bracket rather than space.
             *       The GNAT Bench syntax high-lighter seems to make the same mistake.
             *       To properly detect character literals would need to detect when ' is used for attributes.
             *  */
            contents->previous_lexical_contents_identifier = false;
            contents->previous_lexical_contents_numeric_literal = false;
            append_lexical_char (contents, *ch++);
            append_lexical_char (contents, *ch++);
            num_chars_consumed++;
            append_lexical_char (contents, *ch++);
            num_chars_consumed++;
        }
        else if (isalpha (*ch))
        {
            seperate_identifiers_or_numeric_literals (contents);

            /* Store as lower case the first letter which starts an identifier */
            contents->in_identifier = true;
            contents->previous_identifier_start_index = contents->lexical_length;
            contents->previous_identifier_length = 1;
            append_lexical_char (contents, tolower (*ch));
        }
        else if (isdigit (*ch))
        {
            seperate_identifiers_or_numeric_literals (contents);

            /* Store as the first character of a numeric literal */
            contents->in_numeric_literal = true;
            append_lexical_char (contents, tolower (*ch));
        }
        else if (isspace (*ch))
        {
            /* Whitespace character which isn't part of the lexical contents compared */
        }
        else
        {
            /* Assume punctuation character, which is stored as part of the lexical contents compared.
             * This doesn't attempt to detect characters which are syntax errors. */
            contents->previous_lexical_contents_identifier = false;
            contents->previous_lexical_contents_numeric_literal = false;
            append_lexical_char (contents, *ch);
            if (*ch == ';')
            {
                /* Force a newline as assumed to be end of one statement */
                append_lexical_char (contents, '\n');
            }
        }

        file_index += num_chars_consumed;
    }
}


/**
 * @brief Read the contents of a file for comparison.
 * @param[out] contents The file which has been read, containing both the raw contents and the lexical contents.
 * @param[in] root_directory The root directory in which the file resides.
 * @param[in] source_name The source file name to read, which is relative to the root_directory.
 */
static void read_file_for_comparison (compared_file_contents_t *const contents,
                                      const char *const root_directory, const char *const source_name)
{
    char pathname[PATH_MAX];
    FILE *source_file;
    int rc;

    /* Open source file and gets it length */
    snprintf (pathname, sizeof (pathname), "%s%s", root_directory, source_name);
    source_file = fopen (pathname, "rb");
    if (source_file == NULL)
    {
        printf ("Error: Unable to open %s\n", pathname);
        exit (EXIT_FAILURE);
    }

    rc = fseek (source_file, 0, SEEK_END);
    if (rc != 0)
    {
        printf ("Error: Failed to seek to end of %s\n", pathname);
        exit (EXIT_FAILURE);
    }

    long file_size = ftell (source_file);
    if (file_size < 0)
    {
        printf ("Error: Failed to get size of %s\n", pathname);
        exit (EXIT_FAILURE);
    }

    /* Read the entire contents of the file into memory, and allocate the lexical contents array to be the same maximum length */
    rc = fseek (source_file, 0, SEEK_SET);
    if (rc != 0)
    {
        printf ("Error: Failed to seek to start of %s\n", pathname);
        exit (EXIT_FAILURE);
    }

    contents->file_length = (size_t) file_size;
    contents->file_contents = (char *) malloc (contents->file_length);
    if (contents->file_contents == NULL)
    {
        printf ("Error: Failed to allocate memory to read %s\n", pathname);
        exit (EXIT_FAILURE);
    }

    size_t bytes_read = fread (contents->file_contents, 1, contents->file_length, source_file);
    if (bytes_read != contents->file_length)
    {
        printf ("Error: Failed to read contents of %s\n", pathname);
        exit (EXIT_FAILURE);
    }

    contents->lexical_length = 0;
    contents->lexical_allocated_size = 0;
    contents->lexical_contents = NULL;
    parse_lexical_file_contents (contents);

    (void) fclose (source_file);
}


/**
 * @brief Compare a source file in two trees
 * @param[in] left_tree_root The root of the left source tree for the comparison
 * @param[in] right_tree_root The root of the right source tree for the comparison
 * @param[in] source_name The source file name to compare, present in both the left and right source trees
 * @return The result of comparing the two files.
 */
static file_comparison_t compare_source_files (const char *const left_tree_root, const char *const right_tree_root,
                                               const char *const source_name)
{
    compared_file_contents_t left_contents;
    compared_file_contents_t right_contents;
    file_comparison_t file_comparison;

    read_file_for_comparison (&left_contents, left_tree_root, source_name);
    read_file_for_comparison (&right_contents, right_tree_root, source_name);

    if ((left_contents.file_length == right_contents.file_length) &&
            (memcmp (left_contents.file_contents, right_contents.file_contents, left_contents.file_length) == 0))
    {
        file_comparison = FILE_COMPARISON_BINARY_EQUAL;
    }
    else
    {
        if ((left_contents.lexical_length == right_contents.lexical_length) &&
                (memcmp (left_contents.lexical_contents, right_contents.lexical_contents, left_contents.lexical_length) == 0))
        {
            file_comparison = FILE_COMPARISON_LEXICAL_EQUAL;
        }
        else
        {
            file_comparison = FILE_COMPARISON_DIFFERENT;
        }
    }

    printf ("%.*s", (int) left_contents.lexical_length, left_contents.lexical_contents); //@todo initial test of parsing

    free (left_contents.file_contents);
    free (right_contents.lexical_contents);

    return file_comparison;
}


int main (int argc, char *argv[])
{
    if (argc != 4)
    {
        printf ("Usage: %s <left_source_tree_root> <right_source_tree_root> <result_dir>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    const char *const left_source_tree_root = argv[1];
    const char *const right_source_tree_root = argv[2];
    const char *const result_dir = argv[3];

    /* Get the list of Ada source files in the left and right source trees */
    current_tree = &left_tree;
    current_source_tree_root_prefix_len = 0;
    (void) nftw (left_source_tree_root, tree_walk_callback, MAX_NFTW_OPEN_DESCRIPTORS, FTW_PHYS);
    current_tree = &right_tree;
    current_source_tree_root_prefix_len = 0;
    (void) nftw (right_source_tree_root, tree_walk_callback, MAX_NFTW_OPEN_DESCRIPTORS, FTW_PHYS);

    /* Iterate through the left and right source trees:
     * a. For source files in both source trees perform a comparison of the file contents.
     * b. For source files in only one source tree report that only present in one tree.
     *
     * The order in which iterates over the source trees is determined by the std::string compare() order
     * for the pathnames. The use of the source_file_list_t map to store the paths in the source trees means they
     * are in both in string order, rather than the order nftw() walked the trees.
     */
    source_file_list_t::const_iterator left_it = left_tree.source_list.begin();
    source_file_list_t::const_iterator right_it = right_tree.source_list.begin();
    const std::string *reported_source_name = NULL;
    file_comparison_t file_comparison;
    while ((left_it != left_tree.source_list.end()) || (right_it != right_tree.source_list.end()))
    {
        if (left_it == left_tree.source_list.end())
        {
            /* Right only since reached the end of the left source list */
            file_comparison = FILE_COMPARISON_RIGHT_ONLY;
            reported_source_name = &(*right_it);
            ++right_it;
        }
        else if (right_it == right_tree.source_list.end())
        {
            /* Left only since reached the end of the right source list */
            file_comparison = FILE_COMPARISON_LEFT_ONLY;
            reported_source_name = &(*left_it);
            ++left_it;
        }
        else
        {
            const int compare_rc = left_it->compare (*right_it);

            if (compare_rc == 0)
            {
                /* Same source file in the left and right source list, so compare the file contents */
                file_comparison = compare_source_files (left_tree.root_directory, right_tree.root_directory, left_it->c_str());
                reported_source_name = &(*left_it);
                ++left_it;
                ++right_it;
            }
            else if (compare_rc < 0)
            {
                /* Left only as the left source path is ordered before the right source path */
                file_comparison = FILE_COMPARISON_LEFT_ONLY;
                reported_source_name = &(*left_it);
                ++left_it;
            }
            else
            {
                /* Right only as the left source path is order after the right source path */
                file_comparison = FILE_COMPARISON_RIGHT_ONLY;
                reported_source_name = &(*right_it);
                ++right_it;
            }
        }

        /* @todo initial test of walking the source trees */
        printf ("%s,%s\n", file_comparison_names[file_comparison], reported_source_name->c_str());
    }

    return EXIT_SUCCESS;
}
