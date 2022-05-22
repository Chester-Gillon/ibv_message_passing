/*
 * @file compare_ada_files.cpp
 * @date 21 May 2022
 * @author Chester Gillon
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <set>
#include <string>

#include <ftw.h>


/* Used to contain the relative paths of all source files from one directory root to be compared */
typedef std::set<std::string> source_file_list_t;


/* Maximum number of open file descriptors used by ntfw().
 * Conservative value recommended by https://stackoverflow.com/questions/8436841/how-to-recursively-list-directories-in-c-on-linux
 *
 * sysconf() isn't implemented in mingw, so haven't used the other suggestion of basing upon sysconf(_SC_OPEN_MAX)
 */
#define MAX_NFTW_OPEN_DESCRIPTORS 15


/* The list of source files in two directories trees to be compared */
static source_file_list_t left_source_list;
static source_file_list_t right_source_list;

/* The source list currently being populated by nftw() */
static source_file_list_t *current_source_list = NULL;

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
                        current_source_list->insert (&fpath[current_source_tree_root_prefix_len]);
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
    current_source_list = &left_source_list;
    current_source_tree_root_prefix_len = 0;
    (void) nftw (left_source_tree_root, tree_walk_callback, MAX_NFTW_OPEN_DESCRIPTORS, FTW_PHYS);
    current_source_list = &right_source_list;
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
    source_file_list_t::const_iterator left_it = left_source_list.begin();
    source_file_list_t::const_iterator right_it = right_source_list.begin();
    const std::string *reported_source_name = NULL;
    file_comparison_t file_comparison;
    while ((left_it != left_source_list.end()) || (right_it != right_source_list.end()))
    {
        if (left_it == left_source_list.end())
        {
            /* Right only since reached the end of the left source list */
            file_comparison = FILE_COMPARISON_RIGHT_ONLY;
            reported_source_name = &(*right_it);
            ++right_it;
        }
        else if (right_it == right_source_list.end())
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
                file_comparison = FILE_COMPARISON_DIFFERENT; /* @todo add compare function */
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
