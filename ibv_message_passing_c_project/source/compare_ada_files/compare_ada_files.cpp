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


/* Used to hold the contents of an Ada source file to be compared */
typedef struct
{
    /* The size of the file in bytes */
    size_t file_length;
    /* The raw byte contents of the file. */
    char *file_contents;
    /* The lexical length of the file, after removing comments and whitespace */
    size_t lexical_length;
    /* The lexical contents of the file. Characters outside of string or character literals are converted to lower case
     * to allow for Ada being case-insensitive for identifiers. */
    char *lexical_contents;
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


static void parse_lexical_file_contents (compared_file_contents_t *const contents)
{
    bool in_comment = false;
    bool in_character_literal = false;
    bool in_identifier = false;

    size_t file_index = 0;
    while (file_index < contents->file_length)
    {
        const char *ch = &contents->file_contents[file_index];
        size_t num_chars_consumed = 1;
        const size_t num_remaining_chars = contents->file_length - file_index;

        if (in_comment)
        {
            /* Comment finishes at end of line */
            if (*ch == '\n')
            {
                in_comment = false;
            }
        }
        else if (in_character_literal)
        {
            if ((num_remaining_chars >= 2) && (ch[0] == '\"') && (ch[1] == '\"'))
            {
                /* Found an embedded quote in the character literal */
                contents->lexical_contents[contents->lexical_length++] = *ch++;
                contents->lexical_contents[contents->lexical_length++] = *ch++;
                num_chars_consumed++;
            }
            else if (contents->file_contents[file_index] == '\"')
            {
                /* Found the end of the character literal */
                contents->lexical_contents[contents->lexical_length++] = *ch;
                in_character_literal = false;
            }
            else
            {
                /* One character inside a character literal */
                contents->lexical_contents[contents->lexical_length++] = *ch;
            }
        }
        else if (in_identifier)
        {
            if (isalnum (*ch) || (*ch == '_'))
            {
                /* One character inside an identifier, store as lower case */
                contents->lexical_contents[contents->lexical_length++] = tolower (*ch);
            }
            else
            {
                in_identifier = false;
                if (isspace (*ch))
                {
                    /* The identifier has been terminated by a whitespace character.
                     * Add a single space to the lexical contents to break up identifiers */
                    /* @todo this doesn't produce the same lexical content in the case of spaces between punctuation.
                     *       E.g:
                     *          rx_buffer := ibv_message_bw_interface_h.poll_rx_paths (communication_context);
                     *       v.s:
                     *          rx_buffer:=ibv_message_bw_interface_h.poll_rx_paths(communication_context);
                     *
                     *       To fix this only need to insert spaces between identifiers
                     */
                    contents->lexical_contents[contents->lexical_length++] = ' ';
                }
                else
                {
                    /* The identifier was been terminated by something other than whitespace.
                     * Don't consume the character which is re-evaluated by the next loop iteration. */
                    num_chars_consumed = 0;
                }
            }
        }
        else if ((num_remaining_chars >= 2) && (ch[0] == '-') && (ch[1] == '-'))
        {
            /* Found start of comment, which isn't part of the lexical contents */
            in_comment = true;
            num_chars_consumed++;
        }
        else if (*ch == '\"')
        {
            /* Found start of character literal, which forms part of the lexical contents */
            in_character_literal = true;
            contents->lexical_contents[contents->lexical_length++] = *ch;
        }
        else if ((num_remaining_chars >= 3) && (ch[0] == '\'') && (ch[2] == '\''))
        {
            /* Store a character literal.
             * @todo This simplistic test gets the wrong answer in the case of:
             *          test : Character := Character'(' ');
             *
             *       Since the character literal is consider as open-bracket rather than space.
             *       The GNAT Bench syntax high-lighter seems to make the same mistake.
             *       To properly detect character literals would need to detect when ' is used for attributes.
             *  */
            contents->lexical_contents[contents->lexical_length++] = *ch++;
            contents->lexical_contents[contents->lexical_length++] = *ch++;
            num_chars_consumed++;
            contents->lexical_contents[contents->lexical_length++] = *ch++;
            num_chars_consumed++;
        }
        else if (isalpha (*ch))
        {
            /* Store as lower case the first letter which starts an identifier */
            in_identifier = true;
            contents->lexical_contents[contents->lexical_length++] = tolower (*ch);
        }
        else if (isspace (*ch))
        {
            /* Whitespace character which isn't part of the lexical contents compared */
        }
        else
        {
            /* Assume punctuation character, which is stored as part of the lexical contents compared.
             * This doesn't attempt to detect characters which are syntax errors. */
            contents->lexical_contents[contents->lexical_length++] = *ch++;
        }

        file_index += num_chars_consumed;
    }
}


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
    contents->lexical_length = 0;
    contents->lexical_contents = (char *) malloc (contents->file_length);
    if ((contents->file_contents == NULL) || (contents->lexical_contents == NULL))
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

    parse_lexical_file_contents (contents);

    (void) fclose (source_file);
}


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
