/*
 * @file display_kernel_threads_main.cpp
 * @date 3 Dec 2017
 * @author Chester Gillon
 * @brief Display a summary of the Linux Kernel threads running in the system
 * @details The results are sorted by the priority and name of the thread
 */

/* For cpu_set */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <set>

#include <limits.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sched.h>

#define PROC_FILESYSTEM_ROOT "/proc"

/** Defines the type of a process */
typedef enum
{
    /** Unable to determine the process type */
    UNKNOWN_PROCESS_TYPE,
    /** Used process */
    USER_PROCESS,
    /** A Linux Kernel thread */
    KERNEL_THREAD
} process_type;

/** The attributes for one Kernel thread which are displayed */
#define TASK_COMM_LEN 16
typedef struct
{
    /** The process ID */
    pid_t pid;
    /** The name of the Kernel thread */
    char name[TASK_COMM_LEN + 1];
    /** The scheduling priority where smaller values represent higher priority.
     *  0-99 are real time priorities
     *  100-139 are non real time priorities */
    long int scheduling_priority;
    /** SCHED_* enumeration for the scheduling priority */
    unsigned int scheduling_policy;
    /** Which CPU the Kernel thread was last executed on */
    int cpu_number_last_executed_on;
    /** Amount of time that this process has been scheduled in user mode, measured in clock ticks */
    unsigned long utime;
    /** Amount of time that this process has been scheduled in kernel mode, measured in clock ticks */
    unsigned long stime;
    /** Number of voluntary context switches */
    unsigned long voluntary_ctxt_switches;
    /** Number of involuntary context switches */
    unsigned long nonvoluntary_ctxt_switches;
    /** The CPU affinity for the Kernel thread */
    cpu_set_t affinity;
} kernel_thread_attributes;

/**
 * @brief Comparison function to sort Kernel thread attributes
 */
struct compare_kernel_thread_attributes
{
    bool operator() (const kernel_thread_attributes &a, const kernel_thread_attributes &b) const
    {
        if (a.scheduling_priority != b.scheduling_priority)
        {
            return a.scheduling_priority < b.scheduling_priority;
        }
        else if (strcmp (a.name, b.name) != 0)
        {
            return strcmp (a.name, b.name) < 0;
        }
        else
        {
            return a.pid < b.pid;
        }
    }
};

/** Set of the attributes for all Kernel threads, to be able to sort into display order
 *  where the highest priority Kernel threads are first */
typedef std::set<kernel_thread_attributes, compare_kernel_thread_attributes> kernel_thread_set_t;

/**
 * @brief Obtain the parent process for a specified PID
 * @param[in] pid The process ID to obtain the parent for
 * @param[out] ppid The parent process for pid
 * @return Returns true if the ppid was obtained, or false otherwise
 */
static bool get_pid_parent (const pid_t pid, pid_t *const ppid)
{
    bool got_ppid = false;
    char stat_pathname[PATH_MAX];
    FILE *stat_file;

    sprintf (stat_pathname, "%s/%d/stat", PROC_FILESYSTEM_ROOT, pid);
    stat_file = fopen (stat_pathname, "r");
    if (stat_file != NULL)
    {
        if (fscanf (stat_file, "%*d %*s %*c %d", ppid) == 1)
        {
            got_ppid = true;
        }

        fclose (stat_file);
    }

    return got_ppid;
}

/**
 * @brief Determine if a given PID is a user process or Kernel thread
 * @details This is performed by recursively descending tree of parent processes
 *          to find the root parent process.
 * @param[in] pid The process ID to determine the type for
 * @return The type for pid
 */
static process_type determine_process_type (const pid_t pid)
{
    process_type pid_type = UNKNOWN_PROCESS_TYPE;
    pid_t ppid = -1;

    if (pid == 0)
    {
        /* The root parent process is for that of a Kernel thread */
        pid_type = KERNEL_THREAD;
    }
    else if (pid == 1)
    {
        /* The root parent process is that for init, meaning pid is a user process */
        pid_type = USER_PROCESS;
    }
    else
    {
        if (get_pid_parent (pid, &ppid))
        {
            pid_type = determine_process_type (ppid);
        }
    }

    return pid_type;
}

/**
 * @brief Read the number of context switches for a Kernel thread
 * @param[in] pid The Kernel thread to read the number of context switches for
 * @param[in,out] attrs Updated with the number of context switches for pid
 * @return Returns true if read the number of context switches, or false otherwise
 */
static bool read_pid_context_switches (const pid_t pid, kernel_thread_attributes &attrs)
{
    bool got_voluntary_ctxt_switches = false;
    bool got_nonvoluntary_ctxt_switches = false;
    char status_pathname[PATH_MAX];
    FILE *status_file;
    char line[512];
    unsigned long ctxt_switches;

    sprintf (status_pathname, "%s/%d/status", PROC_FILESYSTEM_ROOT, pid);
    status_file = fopen (status_pathname, "r");
    if (status_file != NULL)
    {
        while (fgets (line, sizeof (line), status_file) != NULL)
        {
            if (sscanf (line, "voluntary_ctxt_switches: %lu", &ctxt_switches) == 1)
            {
                attrs.voluntary_ctxt_switches = ctxt_switches;
                got_voluntary_ctxt_switches = true;
            }
            else if (sscanf (line, "nonvoluntary_ctxt_switches: %lu", &ctxt_switches) == 1)
            {
                attrs.nonvoluntary_ctxt_switches = ctxt_switches;
                got_nonvoluntary_ctxt_switches = true;
            }
        }
        fclose (status_file);
    }

    return got_voluntary_ctxt_switches && got_nonvoluntary_ctxt_switches;
}

/**
 * @brief Read the attributes for one Kernel thread
 * @param[in] pid The process ID to read the attributes for
 * @param[out] attrs The attributes for pid
 * @return Returns true if obtained the process attributes, or false otherwise
 */
static bool read_kernel_thread_attributes (const pid_t pid, kernel_thread_attributes &attrs)
{
    bool got_name = false;
    bool got_stat = false;
    bool got_context_switches = false;
    bool got_cpu_affinity = false;
    char comm_pathname[PATH_MAX];
    char stat_pathname[PATH_MAX];
    FILE *comm_file;
    FILE *stat_file;
    int num_fields;
    int rc;

    attrs.pid = pid;

    /* Read name of the Kernel thread */
    sprintf (comm_pathname, "%s/%d/comm", PROC_FILESYSTEM_ROOT, pid);
    comm_file = fopen (comm_pathname, "r");
    if (comm_file != NULL)
    {
        if (fgets (attrs.name, sizeof (attrs.name), comm_file) != NULL)
        {
            const size_t len = strlen (attrs.name);

            if ((len > 0) && (attrs.name[len - 1] == '\n'))
            {
                attrs.name[len - 1] = '\0';
            }
            got_name = true;
        }
        fclose (comm_file);
    }

    /* Extract the attributes from the /proc/<pid>/stat file */
    sprintf (stat_pathname, "%s/%d/stat", PROC_FILESYSTEM_ROOT, pid);
    stat_file = fopen (stat_pathname, "r");
    if (stat_file != NULL)
    {
        num_fields = fscanf (stat_file,
                "%*d " /* (1) pid  %d */
                "%*s " /* (2) comm  %s */
                "%*c " /* (3) state  %c */
                "%*d " /* (4) ppid  %d */
                "%*d " /* (5) pgrp  %d */
                "%*d " /* (6) session  %d */
                "%*d " /* (7) tty_nr  %d */
                "%*d " /* (8) tpgid  %d */
                "%*u " /* (9) flags  %u */
                "%*u " /* (10) minflt  %lu */
                "%*u " /* (11) cminflt  %lu */
                "%*u " /* (12) majflt  %lu */
                "%*u " /* (13) cmajflt  %lu */
                "%lu " /* (14) utime  %lu
                          Amount of time that this process has been scheduled
                          in user mode, measured in clock ticks (divide by
                          sysconf(_SC_CLK_TCK)).  This includes guest time,
                          guest_time (time spent running a virtual CPU, see
                          below), so that applications that are not aware of
                          the guest time field do not lose that time from
                          their calculations. */
                "%lu " /* (15) stime  %lu
                          Amount of time that this process has been scheduled
                          in kernel mode, measured in clock ticks (divide by
                          sysconf(_SC_CLK_TCK)). */
                "%*d " /* (16) cutime  %ld */
                "%*d " /* (17) cstime  %ld */
                "%ld " /* (18) priority  %ld
                          (Explanation for Linux 2.6) For processes running a
                          real-time scheduling policy (policy below; see
                          sched_setscheduler(2)), this is the negated schedul‐
                          ing priority, minus one; that is, a number in the
                          range -2 to -100, corresponding to real-time priori‐
                          ties 1 to 99.  For processes running under a non-
                          real-time scheduling policy, this is the raw nice
                          value (setpriority(2)) as represented in the kernel.
                          The kernel stores nice values as numbers in the
                          range 0 (high) to 39 (low), corresponding to the
                          user-visible nice range of -20 to 19. */
                "%*d " /* (19) nice  %ld */
                "%*d " /* (20) num_threads  %ld */
                "%*d " /* (21) itrealvalue  %ld */
                "%*u " /* (22) starttime  %llu */
                "%*u " /* (23) vsize  %lu */
                "%*d " /* (24) rss  %ld */
                "%*u " /* (25) rsslim  %lu */
                "%*u " /* (26) startcode  %lu  [PT] */
                "%*u " /* (27) endcode  %lu  [PT] */
                "%*u " /* (28) startstack  %lu  [PT] */
                "%*u " /* (29) kstkesp  %lu  [PT] */
                "%*u " /* (30) kstkeip  %lu  [PT] */
                "%*u " /* (31) signal  %lu */
                "%*u " /* (32) blocked  %lu */
                "%*u " /* (33) sigignore  %lu */
                "%*u " /* (34) sigcatch  %lu */
                "%*u " /* (35) wchan  %lu  [PT] */
                "%*u " /* (36) nswap  %lu */
                "%*u " /* (37) cnswap  %lu */
                "%*d " /* (38) exit_signal  %d  (since Linux 2.1.22) */
                "%d "  /* (39) processor  %d  (since Linux 2.2.8)
                               CPU number last executed on. */
                "%*u " /* (40) rt_priority  %u  (since Linux 2.5.19) */
                "%u ", /* (41) policy  %u  (since Linux 2.5.19)
                          Scheduling policy (see sched_setscheduler(2)).
                          Decode using the SCHED_* constants in linux/sched.h. */
                &attrs.utime,
                &attrs.stime,
                &attrs.scheduling_priority,
                &attrs.cpu_number_last_executed_on,
                &attrs.scheduling_policy);
        if (num_fields == 5)
        {
            /* Convert the scheduling priority to mean zero is the highest real time priority */
            attrs.scheduling_priority += 100;
            got_stat = true;
        }

        fclose (stat_file);
    }

    got_context_switches = read_pid_context_switches (pid, attrs);

    rc = sched_getaffinity (pid, sizeof (attrs.affinity), &attrs.affinity);
    got_cpu_affinity = rc == 0;

    return got_name && got_stat && got_context_switches && got_cpu_affinity;
}

/**
 * @brief Obtain the list of all Kernel threads in the system
 * @param[out] kernel_threads The list of the currently running Kernel threads
 */
static void obtain_kernel_threads (kernel_thread_set_t &kernel_threads)
{
    DIR *proc_dir;
    struct dirent *entry;
    pid_t pid;
    char junk;

    kernel_threads.clear();

    proc_dir = opendir (PROC_FILESYSTEM_ROOT);
    if (proc_dir)
    {
        entry = readdir (proc_dir);
        while (entry != NULL)
        {
            if (sscanf (entry->d_name, "%d%c", &pid, &junk) == 1)
            {
                const process_type pid_type = determine_process_type (pid);
                kernel_thread_attributes attrs;

                switch (pid_type)
                {
                case KERNEL_THREAD:
                    if (read_kernel_thread_attributes (pid, attrs))
                    {
                        kernel_threads.insert (attrs);
                    }
                    else
                    {
                        printf ("Warning: Failed to read attributes for Kernel thread %d\n", pid);
                    }
                    break;

                case USER_PROCESS:
                    /* This program ignores user processes */
                    break;

                case UNKNOWN_PROCESS_TYPE:
                    printf ("Warning: Unable to determine type of pid %d\n", pid);
                    break;
                }
            }

            entry = readdir (proc_dir);
        }

        closedir (proc_dir);
    }
}

/**
 * @brief Convert a scheduling policy enumeration to text for display
 * @param[in] scheduling_policy Enumeration to convert
 * @param[out] text Text for scheduling_policy
 */
static void decode_scheduling_policy (const unsigned int scheduling_policy, char *text)
{
    switch (scheduling_policy)
    {
    case SCHED_FIFO:
        strcpy (text, "FIFO");
        break;

    case SCHED_RR:
        strcpy (text, "RR");
        break;

    case SCHED_OTHER:
        strcpy (text, "OTHER");
        break;

    case SCHED_BATCH:
        strcpy (text, "BATCH");
        break;

    default:
        sprintf (text, "Unknown (%u)", scheduling_policy);
        break;
    }
}

/**
 * @brief Display the CPU affinity
 * @param[in] affinity The CPU affinity to display
 */
static void display_cpu_affinity (const cpu_set_t &affinity)
{
    const int num_cpus_in_affinity = CPU_COUNT (&affinity);
    int num_cpus_displayed;
    int cpu;
    int first_cpu;
    int last_cpu;
    int num_groups_displayed;

    num_cpus_displayed = 0;
    cpu = 0;
    num_groups_displayed = 0;
    while (num_cpus_displayed < num_cpus_in_affinity)
    {
        /* Find the next CPU which is used */
        while ((cpu < CPU_SETSIZE) && !CPU_ISSET (cpu, &affinity))
        {
            cpu++;
        }

        /* Find last CPU in a consecutive group */
        first_cpu = cpu;
        last_cpu = cpu;
        while ((cpu < CPU_SETSIZE) && CPU_ISSET (cpu, &affinity))
        {
            last_cpu = cpu;
            num_cpus_displayed++;
            cpu++;
        }

        /* Display a single or range of CPUs */
        if (num_groups_displayed > 0)
        {
            printf (",");
        }
        if (first_cpu != last_cpu)
        {
            printf ("%d-%d", first_cpu, last_cpu);
        }
        else
        {
            printf ("%d", first_cpu);
        }
        num_groups_displayed++;
    }
}

/**
 * @brief Display all Kernel threads to standard out
 * @param[in] kernel_threads The set of Kernel threads to display
 */
static void display_kernel_threads (const kernel_thread_set_t &kernel_threads)
{
    const long int tick_hz = sysconf(_SC_CLK_TCK);
    const int time_subsec_digits = (int) (log10 ((double) tick_hz) + 1.0);
    kernel_thread_set_t::const_iterator it;
    int field;
    const int num_fields = 10;
    const char *field_names_a[num_fields] = {"",     "",    "",         "",       "Last", "User", "System", "Voluntary", "Involuntary", "CPU"};
    const char *field_names_b[num_fields] = {"Name", "PID", "Priority", "Policy", "CPU",  "time", "time",   "switches",  "switches",    "affinity"};
    const int field_widths[num_fields]    = { 18,     5,     8,          6,        4,      9,      9,        9,           11,            8};

    printf ("%lu Kernel threads\n\n", kernel_threads.size());

    /* Display column headers */
    for (field = 0; field < num_fields; field++)
    {
        printf ("%*s ", field_widths[field], field_names_a[field]);
    }
    printf ("\n");
    for (field = 0; field < num_fields; field++)
    {
        printf ("%*s ", field_widths[field], field_names_b[field]);
    }
    printf ("\n");

    for (it = kernel_threads.begin(); it != kernel_threads.end(); ++it)
    {
        const double user_time_secs = (double) it->utime / (double) tick_hz;
        const double system_time_secs = (double) it->stime / (double) tick_hz;
        char kernel_thread_name[TASK_COMM_LEN + 3];
        char policy_string[128];

        sprintf (kernel_thread_name, "[%s]", it->name);
        decode_scheduling_policy (it->scheduling_policy, policy_string);

        printf ("%*s ", field_widths[0], kernel_thread_name);
        printf ("%*d ", field_widths[1], it->pid);
        printf ("%*ld ", field_widths[2], it->scheduling_priority);
        printf ("%*s " , field_widths[3], policy_string);
        printf ("%*d ", field_widths[4], it->cpu_number_last_executed_on);
        printf ("%*.*f ", field_widths[5], time_subsec_digits, user_time_secs);
        printf ("%*.*f ", field_widths[6], time_subsec_digits, system_time_secs);
        printf ("%*lu ", field_widths[7], it->voluntary_ctxt_switches);
        printf ("%*lu ", field_widths[8], it->nonvoluntary_ctxt_switches);
        display_cpu_affinity (it->affinity);
        printf ("\n");
    }
}

int main (int argc, char *argv[])
{
    kernel_thread_set_t kernel_threads;

    obtain_kernel_threads (kernel_threads);
    display_kernel_threads (kernel_threads);

    return EXIT_SUCCESS;
}
