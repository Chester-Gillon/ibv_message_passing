/*
 * @file ibv_pingpong_measurements_main.c
 * @date 27 Aug 2017
 * @author Chester Gillon
 * @details Run the ibv_rc_pingpong and ibv_uc_pingpong example program under different
 *          combinations of message sizes and polling/blocking measuring the throughput
 *
 *          Runs ibv_rc_pingpong/ibc_uc_pingpong as the server and client on the same PC,
 *          assuming a dual port Infiniband adapter with port 1 looped to port 2.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/resource.h>
#include <limits.h>

#include <glib-unix.h>
#include <glib.h>

/** The different path MTU options which are tested, where 4096 is the max of the adapter */
#define NUM_MTU_OPTIONS 3
static const unsigned int mtu_options[NUM_MTU_OPTIONS] =
{
    1024, 2048, 4096
};

/** The different use events which are tested */
#define NUM_USE_EVENTS_OPTIONS 2
static const bool use_events_options[NUM_USE_EVENTS_OPTIONS] =
{
    false, true
};

/** The range of message sizes which are tested, in increasing powers of two */
#define MIN_MESSAGE_SIZE_LOG2 0
#define MAX_MESSAGE_SIZE_LOG2 24

/** The number of exchanges tested is nominal set from (NOMINAL_DATA_TRANSFERRED_BYTES / message_size),
 *  but limited at min and max values.
 *  NOMINAL_DATA_TRANSFERRED_BYTES is set for around one second (assuming a 40Gbaud link with no protocol overhead) */
#define NOMINAL_DATA_TRANSFERRED_BYTES 4000000000U
#define MIN_EXCHANGES 1000
#define MAX_EXCHANGES 1000000

/** Defines the options used to perform one pingpong test */
typedef struct
{
    /** If true a reliable Infiniband connection is tested by running the ibv_rc_pingpoing program
     *  If false an unreliable Infiniband connection is tested by running the ibv_uc_pingpoing program */
    bool rc_connection;
    /** If true use events, if false poll */
    bool use_events;
    /** The message size in bytes tested */
    unsigned int message_size;
    /** The number of exchanges */
    unsigned int num_exchanges;
    /** Path MTU */
    unsigned int mtu;
} pingpong_options;

/** Used to collect the standard output or standard error from one ibv_rc_pingpong/ibv_uc_pingpong process */
typedef struct
{
    /** The file descriptor written by the process */
    int fd;
    /** Indicates when fd has been closed */
    bool fd_closed;
    /** The channel used to monitor lines available from fd */
    GIOChannel *channel;
    /** The event source ID for channel */
    guint event_source_id;
    /** The number of lines of output which have been collected */
    unsigned int num_lines;
    /** Dynamically allocated lines of output which have been collected */
    char **lines;
} process_output;

/** The state of one ibv_rc_pingpong/ibv_uc_pingpong process */
typedef enum
{
    /** The process has not yet been created */
    PROCESS_NOT_CREATED,
    /** The process has been created */
    PROCESS_CREATED,
    /** The process has been reaped after having exited */
    PROCESS_REAPED
} pingpong_process_state;

/** Contains the context for one ibv_rc_pingpong/ibv_uc_pingpong process */
#define MAX_PROCESS_ARGS 15
typedef struct
{
    /** The state of the process as monitored by this program */
    pingpong_process_state state;
    /** If true this is the server process, otherwise the client process */
    bool is_server;
    /** Contains the dynamically allocated arguments for the process */
    gchar *argv[MAX_PROCESS_ARGS];
    /** The PID of the process */
    GPid pid;
    /** The standard input file descriptor for the process, which is not used */
    gint standard_input;
    /** Used to collect the standard output and standard error for the process */
    process_output standard_output;
    process_output standard_error;
    /** The usage information for the process, valid when state is PROCESS_REAPED */
    struct rusage usage;
    /** The exit status for the process, valid when state is PROCESS_REAPED */
    int exit_status;
} pingpong_process;

/** Contains the context of the pingpong test with one set of options */
typedef struct
{
    /** The options for the test, for use when starting the client */
    pingpong_options options;
    /** The server process which is started first */
    pingpong_process server_process;
    /** The client process which is started after the server process is ready to accept a connection */
    pingpong_process client_process;
    /** Channel to monitor reads from sigchld_read_fd */
    GIOChannel* sigchld_read_channel;
    guint sigchld_read_event_source_id;
    /** Indicates if the total_mlx4_interrupts_at_start and total_mlx4_interrupts_at_end are valid */
    bool interrupt_counts_valid;
    /** The total number of interrupts which have delivered to the Mellanox mlx4 driver, sampled at the
     *  start and end of the test, to detect when the test requires interrupts to be handled. */
    unsigned long total_mlx4_interrupts_at_start;
    unsigned long total_mlx4_interrupts_at_end;
} pingpong_test_context;

/** The context of the test which is currently being performed by test_loopback() */
static GMainLoop *main_loop;
static pingpong_test_context current_test;

/** File descriptors for a pipe which is written to by a signal handler when SIGCHLD occurs,
 *  and read from the mainloop to reap the child process. */
static int sigchld_write_fd;
static int sigchld_read_fd;

static void spawn_pingpong (const pingpong_options *const options, const bool is_server, pingpong_process *const process);

/**
 * @brief Free the standard output or standard error lines which have been collected for a pingpong process
 * @param[in,out] output The process output lines to free
 */
static void free_pingpong_process_output (process_output *const output)
{
    unsigned int line_index;

    for (line_index = 0; line_index < output->num_lines; line_index++)
    {
        g_free (output->lines[line_index]);
    }
    free (output->lines);
    output->lines = NULL;
    output->num_lines = 0;
}

/**
 * @brief Free the results which have been collected for a pingpoing process
 * @param[in,out] process The process to free the results for
 */
static void free_pingpong_process_results (pingpong_process *const process)
{
    int arg_index;

    arg_index = 0;
    while (process->argv[arg_index] != NULL)
    {
        g_free (process->argv[arg_index]);
        process->argv[arg_index] = NULL;
        arg_index++;
    }

    free_pingpong_process_output (&process->standard_output);
    free_pingpong_process_output (&process->standard_error);
}

/**
 * @brief Read the total number of interrupts which have been delivered to the Mellanox mlx4 driver.
 * @details This is used to find under which conditions ibv_rc_pingpong/ibv_uc_pingpong causes interrupts to be generated.
 * @param[out] total_mlx4_interrupts The total number of interrupts delivered to the Mellanox mlx4 driver.
 * @return Returns true if total_interrupts is valid, or false if didn't find the Mellanox mlx4 driver in the list of interrupts
 */
static bool get_total_mlx4_interrupts (unsigned long *const total_mlx4_interrupts)
{
    const char *whitespace_delim = " \t";
    bool results_valid = false;
    int interrupts_fd;
    GIOChannel *interrupts_channel;
    GIOStatus status;
    gchar *line;
    GError *error = NULL;
    unsigned int num_cpus;
    int cpu;
    char *saveptr;
    char *token;
    unsigned long current_interrupt_id_total;
    bool total_valid;
    unsigned int cpu_index;
    unsigned long interrupt_count;
    char junk;

    *total_mlx4_interrupts = 0;
    interrupts_fd = open ("/proc/interrupts", 0, O_RDONLY);
    if (interrupts_fd != -1)
    {
        interrupts_channel = g_io_channel_unix_new (interrupts_fd);

        /* Parse the first line of /proc/interrupts to obtain the number of CPUs */
        num_cpus = 0;
        status = g_io_channel_read_line (interrupts_channel, &line, NULL, NULL, &error);
        if (status == G_IO_STATUS_NORMAL)
        {
            saveptr = NULL;
            token = strtok_r (line, whitespace_delim, &saveptr);
            while ((token != NULL) && (sscanf (token, "CPU%d", &cpu) == 1))
            {
                num_cpus++;
                token = strtok_r (NULL, whitespace_delim, &saveptr);
            }
            g_free (line);
        }

        if (num_cpus > 0)
        {
            status = g_io_channel_read_line (interrupts_channel, &line, NULL, NULL, &error);
            while (status == G_IO_STATUS_NORMAL)
            {
                /* Skip the interrupt ID */
                saveptr = NULL;
                token = strtok_r (line, whitespace_delim, &saveptr);

                /* Sum the number of interrupts delivered to all CPUs for this interrupt ID */
                current_interrupt_id_total = 0;
                total_valid = true;
                for (cpu_index = 0; total_valid && (cpu_index < num_cpus); cpu_index++)
                {
                    token = strtok_r (NULL, whitespace_delim, &saveptr);
                    total_valid = (token != NULL) && (sscanf (token, "%lu%c", &interrupt_count, &junk) == 1);
                    if (total_valid)
                    {
                        current_interrupt_id_total += interrupt_count;
                    }
                }

                /* saveptr is the remainder of the line after the interrupt counts; which contains multiple white space
                 * delimited fields containing a description of the interrupt and the attached driver. */
                if (total_valid && (strstr (saveptr, "mlx4") != NULL))
                {
                    /* This interrupt ID is serviced by the mlx4 driver */
                    (*total_mlx4_interrupts) += current_interrupt_id_total;
                    results_valid = true;
                }

                g_free (line);
                status = g_io_channel_read_line (interrupts_channel, &line, NULL, NULL, &error);
            }
        }

        g_io_channel_unref (interrupts_channel);
        close (interrupts_fd);
    }

    return results_valid;
}

/**
 * @brief Handler for SIGCHLD which writes to a pipe to to notify the main loop
 * @param[in] sig Signal number (not used)
 */
static void handle_sigchld (int sig)
{
    ssize_t num_written;
    char data = 'S';

    num_written = write (sigchld_write_fd, &data, sizeof (data));
    if (num_written != 1)
    {
        fprintf (stderr, "write from signal handler failed\n");
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Install a signal handler which is used to write to a pipe to notify the main loop when a SIGCHLD occurs
 * @details The reason for installing our own signal handler rather than use g_child_watch_add() is to be able to
 *          use wait4() to reap the child process to obtain the usage information of the child.
 */
static void install_sigchld_handler (void)
{
    struct sigaction sa;
    int rc;
    int sigchld_fds[2];
    gboolean status;
    GError *error = NULL;

    status = g_unix_open_pipe (sigchld_fds, FD_CLOEXEC, &error);
    if (!status)
    {
        fprintf (stderr, "g_unix_open_pipe failed:\n%s\n", error->message);
        exit (EXIT_FAILURE);
    }
    sigchld_read_fd = sigchld_fds[0];
    sigchld_write_fd = sigchld_fds[1];

    sa.sa_handler = handle_sigchld;
    sigemptyset (&sa.sa_mask);
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
    rc = sigaction (SIGCHLD, &sa, 0);
    if (rc != 0)
    {
        fprintf (stderr, "sigaction (SIGCHLD) failed\n");
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Abort the test if an ibv_rc_pingpong/ibv_uc_pingpong process indicates it as exited with an error status
 * @param[in] process The process to check for failure
 */
static void abort_on_process_failure (const pingpong_process *const process)
{
    unsigned int arg_index;
    unsigned int line_index;
    bool process_failed = (process->state == PROCESS_REAPED) &&
            (!WIFEXITED (process->exit_status) || (WEXITSTATUS (process->exit_status) != 0)) &&
            process->standard_output.fd_closed && process->standard_error.fd_closed;

    if (process_failed)
    {
        printf ("Process started with:\n");
        arg_index = 0;
        while (process->argv[arg_index] != NULL)
        {
            printf (" %s", process->argv[arg_index]);
            arg_index++;
        }
        printf ("\nFailed with:\n");
        for (line_index = 0; line_index < process->standard_error.num_lines; line_index++)
        {
            printf (" %s", process->standard_error.lines[line_index]);
        }
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Determine if an ibv_rc_pingpong/ibv_uc_pingpong process has exited with a successful completion status.
 * @param[in] process The process to check for success
 * @return Returns true the process has exited with a successful completion status.
 *         Returns false if failed, or has not yet exited
 */
static bool process_successful_completion (const pingpong_process *const process)
{
    return (process->state == PROCESS_REAPED) && WIFEXITED (process->exit_status) && (WEXITSTATUS (process->exit_status) == 0) &&
            process->standard_output.fd_closed && process->standard_error.fd_closed;
}

/**
 * @brief Called from a Glib callback to check if the current test has passed or failed
 * @details Abort the program if a ibv_rc_pingpong/ibv_uc_pingpong process has failed.
 *          Cause the Glib main loop to exit when both server and client ibv_rc_pingpong/ibv_uc_pingpong processes have successfully completed.
 *          Also spawns the client process once the server is ready for a connection.
 */
static void check_for_test_pass_or_failure (void)
{
    abort_on_process_failure (&current_test.server_process);
    abort_on_process_failure (&current_test.client_process);

    if (process_successful_completion (&current_test.server_process) &&
        process_successful_completion (&current_test.client_process))
    {
        g_main_loop_quit (main_loop);
    }

    if ((current_test.server_process.state == PROCESS_CREATED) && (current_test.client_process.state == PROCESS_NOT_CREATED) &&
        (current_test.server_process.standard_output.num_lines == 1))
    {
        /* Spawn the client once the server has generated one line of output, meaning the server is ready for the client to connect */
        spawn_pingpong (&current_test.options, false, &current_test.client_process);
    }
}

/**
 * @brief Store the exit status for a process, if the pid matches one of the pingpong processes
 * @param[in,out] process The pingpong process to check for a match against
 * @param[in] pid The PID of the process which has exited
 * @param[in] exit_status The exit status of the process
 * @param[in] usage The usage of the process which has exited
 */
static void store_process_exit_status (pingpong_process *const process,
                                       const pid_t pid, const int exit_status, const struct rusage *const usage)
{
    if ((process->state == PROCESS_CREATED) && (process->pid == pid))
    {
        process->usage = *usage;
        process->exit_status = exit_status;
        process->state = PROCESS_REAPED;
    }
}

/**
 * @brief Callback for when sigchld_read_fd is readable, to check if one of the pingpong processes has exited
 * @param[in] source The GIOChannel event source
 * @param[in] condition The condition which has been satisfied
 * @param[in,out] data Points to the pingpong_test_context for the current test
 * @return Returns TRUE to indicate the event source is still required.
 */
static gboolean sigchld_watch (GIOChannel *source, GIOCondition condition, gpointer data)
{
    pingpong_test_context *const context = (pingpong_test_context *) data;
    char buf;
    gsize bytes_read = 0;
    struct rusage usage;
    pid_t pid;
    int exit_status;

    if (g_io_channel_read_chars (context->sigchld_read_channel, &buf, 1, &bytes_read, NULL) == G_IO_STATUS_NORMAL)
    {
        do
        {
            pid = wait4 ((pid_t)(-1), &exit_status, WNOHANG, &usage);
            if (pid > 0)
            {
                store_process_exit_status (&context->server_process, pid, exit_status, &usage);
                store_process_exit_status (&context->client_process, pid, exit_status, &usage);
            }
        } while (pid > 0);
    }

    check_for_test_pass_or_failure ();

    return TRUE;
}

/**
 * @brief Glib callback for a process standard output or standard error
 * @details This appends lines of output to the process status, and closes the channel when the process has exited
 * @param[in] source The GIOChannel event source
 * @param[in] condition The condition which has been satisfied, expected to be G_IO_IN or G_IO_HUP
 * @param[in,out] data Points to the process_output to append lines to
 * @return Returns false once the channel has been closed
 */
static gboolean process_output_watch (GIOChannel *source, GIOCondition condition, gpointer data)
{
    process_output *const output = (process_output *) data;
    GIOStatus status;
    gchar *line = NULL;
    GError *error = NULL;

    if (!output->fd_closed && ((condition & G_IO_IN) != 0))
    {
        status = g_io_channel_read_line (source, &line, NULL, NULL, &error);
        switch (status)
        {
        case G_IO_STATUS_NORMAL:
            output->lines = realloc (output->lines, (output->num_lines + 1) * sizeof (gchar *));
            output->lines[output->num_lines] = line;
            output->num_lines++;
            break;

        case G_IO_STATUS_EOF:
            g_io_channel_unref (source);
            close (output->fd);
            output->fd_closed = true;
            break;

        default:
            break;
        }
    }

    if (!output->fd_closed && ((condition & G_IO_HUP) != 0))
    {
        g_io_channel_unref (source);
        close (output->fd);
        output->fd_closed = true;
    }

    check_for_test_pass_or_failure ();

    return !output->fd_closed;
}

/**
 * @brief Initialise the recording of standard output or standard error for a process
 * @param[out] output Initialised to record the output, via Glib callbacks
 * @param[in] fd The file descriptor to record the output for
 */
static void initialise_process_output (process_output *const output, const int fd)
{
    GIOStatus status;
    GError *error = NULL;

    output->fd = fd;
    output->fd_closed = false;
    output->num_lines = 0;
    output->lines = NULL;
    output->channel = g_io_channel_unix_new (output->fd);
    output->event_source_id = g_io_add_watch (output->channel, G_IO_IN | G_IO_HUP, process_output_watch, output);
    status = g_io_channel_set_encoding (output->channel, NULL, &error);
    if (status != G_IO_STATUS_NORMAL)
    {
        fprintf (stderr, "g_io_channel_set_encoding failed with:\n%s\n", error->message);
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Spawn a ibv_rc_pingpong/ibv_uc_pingpong process
 * @details The process is spawned via the stdbuf command to set line buffering. This is so that can read the
 *          output of the server process to detect when the server is ready to accept the client.
 * @param[in] options The test options to be passed to the ibv_rc_pingpong/ibv_uc_pingpong process via command line options
 * @param[in] is_server If true spawning the server, if false the client
 * @param[out] process Initialised with the information for the spawned process
 */
static void spawn_pingpong (const pingpong_options *const options, const bool is_server, pingpong_process *const process)
{
    const int infiniband_port = is_server ? 1 : 2;
    gboolean status;
    int argc;
    int stderr_fd;
    int stdout_fd;
    GError *error = NULL;

    /* Build the command line arguments for the pingpong process */
    argc = 0;
    process->argv[argc++] = g_strdup_printf ("stdbuf");
    process->argv[argc++] = g_strdup_printf ("-oL");
    process->argv[argc++] = g_strdup_printf ("-eL");
    process->argv[argc++] = g_strdup_printf (options->rc_connection ? "ibv_rc_pingpong" : "ibv_uc_pingpong");
    if (!is_server)
    {
        /* The client and server are both running on the local machine */
        process->argv[argc++] = g_strdup_printf ("localhost");
    }
    process->argv[argc++] = g_strdup_printf ("--ib-port=%d", infiniband_port);
    if (options->use_events)
    {
        process->argv[argc++] = g_strdup_printf ("--events");
    }
    process->argv[argc++] = g_strdup_printf ("--size=%u", options->message_size);
    process->argv[argc++] = g_strdup_printf ("--iters=%u", options->num_exchanges);
    process->argv[argc++] = g_strdup_printf ("--mtu=%u", options->mtu);
    process->argv[argc] = NULL;

    /* Spawn the pingpong process */
    process->is_server = is_server;
    status = g_spawn_async_with_pipes (NULL, process->argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH, NULL, NULL,
                                       &process->pid, &process->standard_input, &stdout_fd, &stderr_fd, &error);
    if (!status)
    {
        fprintf (stderr, "Spawn of pingpong %s failed:\n%s\n", is_server ? "server" : "client", error->message);
        exit (EXIT_FAILURE);
    }
    process->state = PROCESS_CREATED;

    initialise_process_output (&process->standard_output, stdout_fd);
    initialise_process_output (&process->standard_error, stderr_fd);

}

/**
 * @brief Report the detailed test results for one pingpong process
 * @param[in] details_file Where to write the results to
 * @param[in] process The pingpong process to write the results to
 */
static void report_process_test_details (FILE *const details_file, const pingpong_process *const process)
{
    unsigned int line_index;

    fprintf (details_file, "\n%s  voluntary_ctxt_switches=%lu  user time=%lu.%06lu  system time=%lu.%06lu\n",
             process->is_server ? "Server" : "Client",
             process->usage.ru_nvcsw,
             process->usage.ru_utime.tv_sec, process->usage.ru_utime.tv_usec,
             process->usage.ru_stime.tv_sec, process->usage.ru_stime.tv_usec);

    for (line_index = 0; line_index < process->standard_output.num_lines; line_index++)
    {
        fprintf (details_file, " %s", process->standard_output.lines[line_index]);
    }
}

/**
 * @brief Report the detailed test results for one test
 * @param[in] options The test options used
 * @param[in] details_file Where to write the results to
 * @param[in] results The test results to report
 */
static void report_test_details (const pingpong_options *const options, FILE *const details_file,
                                 const pingpong_test_context *const results)
{
    fprintf (details_file, "Testing %s using use_events=%d message_size=%u num_exchanges=%u mtu=%u\n",
             options->rc_connection ? "ibv_rc_pingpong" : "ibv_uc_pingpong",
             options->use_events, options->message_size, options->num_exchanges, options->mtu);
    report_process_test_details (details_file, &results->server_process);
    report_process_test_details (details_file, &results->client_process);

    if (results->interrupt_counts_valid)
    {
        fprintf (details_file, "\nTest generated %lu mlx4 interrupts (%lu -> %lu)\n",
                 results->total_mlx4_interrupts_at_end - results->total_mlx4_interrupts_at_start,
                 results->total_mlx4_interrupts_at_start, results->total_mlx4_interrupts_at_end);
    }

    fprintf (details_file, "\n===============================================================================\n");
}

/**
 * @brief Report the summary test results for one pingpong process
 * @param[in] details_file Where to write the results to
 * @param[in] process The pingpong process to write the results to
 */
static void report_process_test_summary (FILE *const summary_file, const pingpong_process *const process)
{
    unsigned int line_index;
    bool found_mbits_per_sec;
    char *mbits_per_sec = NULL;

    /* Report the Mbit/sec value using the same text was from the standard output of the ibv_rc_pingpong/ibv_uc_pingpong process */
    found_mbits_per_sec = false;
    for (line_index = 0; !found_mbits_per_sec && (line_index < process->standard_output.num_lines); line_index++)
    {
        found_mbits_per_sec = sscanf (process->standard_output.lines[line_index],
                                      "%*u bytes in %*f seconds = %ms Mbit/sec", &mbits_per_sec) == 1;
    }
    if (found_mbits_per_sec)
    {
        fprintf (summary_file, "%s,", mbits_per_sec);
        free (mbits_per_sec);
    }
    else
    {
        fprintf (summary_file, ",");
    }

    fprintf (summary_file, "%lu,%lu.%06lu,%lu.%06lu,",
             process->usage.ru_nvcsw,
             process->usage.ru_utime.tv_sec, process->usage.ru_utime.tv_usec,
             process->usage.ru_stime.tv_sec, process->usage.ru_stime.tv_usec);
}

/**
 * @brief Report the summary test results for one test
 * @param[in] options The test options used
 * @param[in] details_file Where to write the results to
 * @param[in] results The test results to report
 */
static void report_test_summary (const pingpong_options *const options, FILE *const summary_file,
                                 const pingpong_test_context *const results)
{
    fprintf (summary_file,"%s,%d,%u,%u,%u,",
             options->rc_connection ? "ibv_rc_pingpong" : "ibv_uc_pingpong",
             options->use_events, options->message_size, options->num_exchanges, options->mtu);
    if (results->interrupt_counts_valid)
    {
        fprintf (summary_file, "%lu,", results->total_mlx4_interrupts_at_end - results->total_mlx4_interrupts_at_start);
    }
    else
    {
        fprintf (summary_file, ",");
    }
    report_process_test_summary (summary_file, &results->server_process);
    report_process_test_summary (summary_file, &results->client_process);
    fprintf (summary_file, "\n");
}

/**
 * @details Run the ibv_rc_pingpong/ibv_uc_pingpong server and client processes for one set of options, and report the
 *          detailed and summary test results
 * @param[in] options The test options used
 * @param[in] details_file Where to write the detailed results to
 * @param[in] summary_file Where to write the detailed results to
 */
static void test_loopback (const pingpong_options *const options, FILE *const details_file, FILE *const summary_file)
{
    GError *error = NULL;
    GIOStatus status;

    memset (&current_test, 0, sizeof (current_test));
    current_test.interrupt_counts_valid = get_total_mlx4_interrupts (&current_test.total_mlx4_interrupts_at_start);

    /* Initialise an unbuffered channel for handling when a SIGCHLD has occurred when a process has exited */
    current_test.sigchld_read_channel = g_io_channel_unix_new (sigchld_read_fd);
    status = g_io_channel_set_encoding (current_test.sigchld_read_channel, NULL, &error);
    if (status != G_IO_STATUS_NORMAL)
    {
        fprintf (stderr, "g_io_channel_set_encoding failed:\n%s\n", error->message);
    }
    g_io_channel_set_buffered (current_test.sigchld_read_channel, false);
    current_test.sigchld_read_event_source_id = g_io_add_watch (current_test.sigchld_read_channel, G_IO_IN,
            sigchld_watch, &current_test);

    /* Spawn the server process. The client process gets spawned from a callback in the main loop */
    current_test.options = *options;
    spawn_pingpong (options, true, &current_test.server_process);

    /* Wait for the server and client ibv_rc_pingpong/ibv_uc_pingpong processes to run to completion */
    main_loop = g_main_loop_new (NULL, FALSE);
    g_main_loop_run (main_loop);
    g_main_loop_unref (main_loop);
    g_io_channel_unref (current_test.sigchld_read_channel);
    g_source_remove (current_test.sigchld_read_event_source_id);
    current_test.interrupt_counts_valid &= get_total_mlx4_interrupts (&current_test.total_mlx4_interrupts_at_end);

    /* Report and then free the results */
    report_test_details (options, details_file, &current_test);
    report_test_summary (options, summary_file, &current_test);
    free_pingpong_process_results (&current_test.server_process);
    free_pingpong_process_results (&current_test.client_process);
}

int main (int argc, char *argv[])
{
    pingpong_options options;
    char *results_dir = argv[1];
    char details_pathname[PATH_MAX];
    char summary_pathname[PATH_MAX];
    FILE *details_file;
    FILE *summary_file;
    unsigned int mtu_option_index;
    unsigned int use_events_option_index;
    unsigned int message_size_log2;

    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s <results_directory>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    sprintf (details_pathname, "%s/ibv_pingpong_measurements_detail.txt", results_dir);
    details_file = fopen (details_pathname, "w");
    if (details_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", details_pathname);
        exit (EXIT_FAILURE);
    }

    sprintf (summary_pathname, "%s/ibv_pingpong_measurements_summary.csv", results_dir);
    summary_file = fopen (summary_pathname, "w");
    if (summary_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", summary_pathname);
        exit (EXIT_FAILURE);
    }
    fprintf (summary_file, "Program,use_events,message_size,num_exchanges,MTU,Num mlx4 interrupts,");
    fprintf (summary_file, "Server Mbit/sec,Server voluntary_ctxt_switches,Server user time,Server system time,");
    fprintf (summary_file, "Server Mbit/sec,Client voluntary_ctxt_switches,Client user time,Client system time\n");

    install_sigchld_handler ();

    for (message_size_log2 = MIN_MESSAGE_SIZE_LOG2; message_size_log2 <= MAX_MESSAGE_SIZE_LOG2; message_size_log2++)
    {
        options.message_size = 1 << message_size_log2;
        options.num_exchanges = NOMINAL_DATA_TRANSFERRED_BYTES / options.message_size;
        if (options.num_exchanges < MIN_EXCHANGES)
        {
            options.num_exchanges = MIN_EXCHANGES;
        }
        else if (options.num_exchanges > MAX_EXCHANGES)
        {
            options.num_exchanges = MAX_EXCHANGES;
        }
        for (mtu_option_index = 0; mtu_option_index < NUM_MTU_OPTIONS; mtu_option_index++)
        {
            options.mtu = mtu_options[mtu_option_index];
            for (use_events_option_index = 0; use_events_option_index < NUM_USE_EVENTS_OPTIONS; use_events_option_index++)
            {
                options.use_events = use_events_options[use_events_option_index];
                options.rc_connection = false;
                test_loopback (&options, details_file, summary_file);
                options.rc_connection = true;
                test_loopback (&options, details_file, summary_file);
            }
        }
    }

    fclose (details_file);
    fclose (summary_file);

    return EXIT_SUCCESS;
}
