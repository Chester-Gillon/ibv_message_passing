/*
 * @file find_processes_using_file.c
 * @date 15 Apr 2023
 * @author Chester Gillon
 * @brief Program to investigate finding processes which have named file open
 * @details
 *  Created to investigate
 *  https://unix.stackexchange.com/questions/742548/what-is-most-efficient-way-to-get-the-pid-of-the-process-that-is-using-a-file-on
 *
 *  Two methods can be used:
 *  a. Iterate over /proc/<pid>/fd. I.e. search by PID
 *  b. Iterate over /proc/<pid>/task/<tid>/fd. I.e. search by TID
 *
 *  Searching by PID is faster. However, if only the main thread in a process has exited, leaving other threads running
 *  then searching by TID is required to find a match against the processes in which the main thread has exited.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#include <limits.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>


/* Command line argument which enables scanning TIDs rather than PIDs for file references */
static bool arg_scan_tids;

/* Command line arguments which specify the list of filenames to search for references to */
static char **arg_filenames;
static int arg_num_filenames;

/* Command line argument which controls how filenames are matched */
typedef enum
{
    FILENAME_MATCH_ABSOLUTE,
    FILENAME_MATCH_PREFIX,
    FILENAME_MATCH_SUBSTRING
} filename_match_t;
static filename_match_t arg_filename_match = FILENAME_MATCH_ABSOLUTE;

/* Statistics on the total number of filenames compared */
static int num_filenames_compared;

/* Statistics on the total number of EACCES errors while trying to scan filename references.
 * This can occur if the user running the program doesn't have permission to open the fd directory.
 * E.g. when the process with the fd directory is a different used. */
static int num_eacces_errors;


/**
 * @brief Parse the command line arguments, storing the results in global variables
 * @param[in] argc, argv Arguments passed to main
 */
static void parse_command_line_arguments (int argc, char *argv[])
{
    const char *const optstring = "tm:?";
    int option;

    option = getopt (argc, argv, optstring);
    while (option != -1)
    {
        switch (option)
        {
        case 't':
            arg_scan_tids = true;
            break;

        case 'm':
            if (strcmp (optarg, "absolute") == 0)
            {
                arg_filename_match = FILENAME_MATCH_ABSOLUTE;
            }
            else if (strcmp (optarg, "prefix") == 0)
            {
                arg_filename_match = FILENAME_MATCH_PREFIX;
            }
            else if (strcmp (optarg, "substring") == 0)
            {
                arg_filename_match = FILENAME_MATCH_SUBSTRING;
            }
            else
            {
                printf ("Error: Invalid type of match \"%s\"\n", optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case '?':
        default:
            printf ("Usage: %s [-t] [-m absolute|prefix|substring] filename1 [.. filenameN]\n", argv[0]);
            printf ("  -t enables scanning TIDs, rather than PIDs, to find references to files\n");
            printf ("  -m specifies how much of the filename text is compared\n");
            exit (EXIT_FAILURE);
        }
        option = getopt (argc, argv, optstring);
    }

    arg_filenames = &argv[optind];
    arg_num_filenames = argc - optind;
    if (arg_num_filenames == 0)
    {
        printf ("No filenames specified\n");
        exit (EXIT_FAILURE);
    }
}


/*
 * @brief Scan a fd directory, reporting if any of the files reference the names given on the command line options
 * @param[in] fd_dir_to_scan The directory to scan, which may be at either:
 *            - /proc/pid/fd meaning scanning PIDs
 *            - /proc/pid/task/tid/fd meaning scanning TIDs
 * @returns Returns true if a file referenced any names given on the command line options
 */
static bool scan_fd_dir_for_filename_matches (const char *const fd_dir_to_scan)
{
    bool matched = false;
    int saved_errno;
    DIR *fd_dir;
    struct dirent *fd_ent;
    int rc;
    char fd_symlnk[PATH_MAX];
    char fd_target[PATH_MAX + 1];
    size_t fd_target_len;

    errno = 0;
    fd_dir = opendir (fd_dir_to_scan);
    saved_errno = errno;
    if (fd_dir != NULL)
    {
        fd_ent = readdir (fd_dir);
        while (!matched && (fd_ent != NULL))
        {
            if (fd_ent->d_type == DT_LNK)
            {
                snprintf (fd_symlnk, sizeof (fd_symlnk), "%s/%s", fd_dir_to_scan, fd_ent->d_name);
                errno = 0;
                fd_target_len = readlink (fd_symlnk, fd_target, sizeof (fd_target) - 1);
                saved_errno = errno;
                if (fd_target_len > 0)
                {
                    fd_target[fd_target_len] = '\0';
                    for (int filename_index = 0; !matched && (filename_index < arg_num_filenames); filename_index++)
                    {
                        const char *const compared_filename = arg_filenames[filename_index];

                        switch (arg_filename_match)
                        {
                        case FILENAME_MATCH_ABSOLUTE:
                            matched = strcmp (fd_target, compared_filename) == 0;
                            break;

                        case FILENAME_MATCH_PREFIX:
                            matched = strncmp (fd_target, compared_filename, strlen (compared_filename)) == 0;
                            break;

                        case FILENAME_MATCH_SUBSTRING:
                            matched = strstr (fd_target, compared_filename) != NULL;
                            break;
                        }
                        if (matched)
                        {
                            printf ("%s -> %s\n", fd_symlnk, fd_target);
                        }
                    }
                    num_filenames_compared++;
                }
                else if (saved_errno == EACCES)
                {
                    num_eacces_errors++;
                }
            }

            fd_ent = readdir (fd_dir);
        }

        rc = closedir (fd_dir);
        if (rc != 0)
        {
            printf ("closedir() failed\n");
        }
    }
    else if (saved_errno == EACCES)
    {
        num_eacces_errors++;
    }

    return matched;
}


/**
 * @brief Search /proc/<pid>/fd directories for references to filenames specified on the command line
 */
static void find_pid_file_references (void)
{
    DIR *proc_dir;
    struct dirent *proc_ent;
    int rc;
    pid_t pid;
    char junk;
    char fd_dir_to_scan[PATH_MAX];

    proc_dir = opendir ("/proc");
    if (proc_dir != NULL)
    {
        proc_ent = readdir (proc_dir);
        while (proc_ent != NULL)
        {
            if ((proc_ent->d_type == DT_DIR) && (sscanf (proc_ent->d_name, "%d%c", &pid, &junk) == 1))
            {
                snprintf (fd_dir_to_scan, sizeof (fd_dir_to_scan), "/proc/%s/fd", proc_ent->d_name);
                (void) scan_fd_dir_for_filename_matches (fd_dir_to_scan);
            }

            proc_ent = readdir (proc_dir);
        }

        rc = closedir (proc_dir);
        if (rc != 0)
        {
            printf ("closedir() failed\n");
        }
    }
}


/**
 * @brief Search /proc/<pid>/task/<tid>/fd directories for references to filenames specified on the command line
 * @details Since all threads in a process share the file descriptors, just reports the first TID on which a
 *          match is found
 */
static void find_tid_file_references (void)
{
    DIR *proc_dir;
    struct dirent *proc_ent;
    DIR *task_dir;
    struct dirent *task_ent;
    int rc;
    pid_t pid;
    pid_t tid;
    char task_dirname[PATH_MAX];
    char fd_dir_to_scan[PATH_MAX];
    char junk;
    size_t num_chars;
    bool matched;

    proc_dir = opendir ("/proc");
    if (proc_dir != NULL)
    {
        proc_ent = readdir (proc_dir);
        while (proc_ent != NULL)
        {
            if ((proc_ent->d_type == DT_DIR) && (sscanf (proc_ent->d_name, "%d%c", &pid, &junk) == 1))
            {
                matched = false;
                snprintf (task_dirname, sizeof (task_dirname), "/proc/%s/task", proc_ent->d_name);
                task_dir = opendir (task_dirname);
                if (task_dir != NULL)
                {
                    task_ent = readdir (task_dir);
                    while (!matched && (task_ent != NULL))
                    {
                        if ((task_ent->d_type == DT_DIR) && (sscanf (task_ent->d_name, "%d%c", &tid, &junk) == 1))
                        {
                            /* use of num_chars suppresses -Wformat-truncation, as suggested by
                             * https://stackoverflow.com/a/70938456/4207678 */
                            num_chars = sizeof (fd_dir_to_scan);
                            snprintf (fd_dir_to_scan, num_chars, "%s/%s/fd", task_dirname, task_ent->d_name);
                            matched = scan_fd_dir_for_filename_matches (fd_dir_to_scan);
                        }

                        task_ent = readdir (task_dir);
                    }

                    rc = closedir (task_dir);
                    if (rc != 0)
                    {
                        printf ("closedir() failed\n");
                    }
                }
            }

            proc_ent = readdir (proc_dir);
        }

        rc = closedir (proc_dir);
        if (rc != 0)
        {
            printf ("closedir() failed\n");
        }
    }

}


int main (int argc, char *argv[])
{
    parse_command_line_arguments (argc, argv);

    if (arg_scan_tids)
    {
        find_tid_file_references ();
    }
    else
    {
        find_pid_file_references ();
    }

    /* Give an indication of the number of comparisons performed */
    printf ("\nnum_filenames_compared=%d num_eacces_errors=%d\n", num_filenames_compared, num_eacces_errors);

    return EXIT_SUCCESS;
}
