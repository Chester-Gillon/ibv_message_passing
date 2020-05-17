/*
 * @file ibv_spawn_workers_and_controller.c
 * @date 23 Jun 2019
 * @author Chester Gillon
 * @details Program to spawn the Infiniband controller and worker processes, to investigate debugging forked processes
 *          using Eclipse and GDB.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"


/** The number of processes spawned */
#define NUM_PROCESSES 4


/** Used to spawn one test process */
typedef struct
{
    /** The filename of the process to spawn */
    const char *executable_file;
    /** The arguments for the process */
    char *argv[4];
    /** The identity of the spawned process */
    pid_t pid;
    /** Set true when the child process has exited and been reaped */
    bool reaped;
} test_process_t;


/** Defines the test processes which are spawned, with the controller and worker processes configured to be running in the
 *  same PC. The current working directory needs to be set to one which has sym-links to the C and Ada executables */
static test_process_t test_processes[NUM_PROCESSES] =
{
    { /* Ada controller process */
        .executable_file = "ibv_controller_process_main",
        .argv = {"ibv_controller_process_main", "false", NULL}
    },
    { /* C worker processes */
        .executable_file = "ibv_worker_process",
        .argv = {"ibv_worker_process", "1", "0", NULL}
    },
    {
        .executable_file = "ibv_worker_process",
        .argv = {"ibv_worker_process", "2", "0", NULL}
    },
    {
        .executable_file = "ibv_worker_process",
        .argv = {"ibv_worker_process", "3", "0", NULL}
    }
};


int main (int argc, char *argv[])
{
    int process_index;
    pid_t pid;
    int num_active_processes;
    int rc;
    siginfo_t info;

    /* Spawn the child processes */
    num_active_processes = 0;
    for (process_index = 0; process_index < NUM_PROCESSES; process_index++)
    {
        test_process_t *const process = &test_processes[process_index];

        pid = fork ();
        if (pid == 0)
        {
            /* In child */
            rc = execv (process->executable_file, process->argv);

            /* An error has occurred if execvp returns */
            fprintf (stderr, "execvp (%s) failed : %s\n", process->executable_file, strerror (errno));
        }
        else
        {
            /* In parent */
            CHECK_ASSERT (pid > 0);
            process->pid = pid;
            process->reaped = false;
            num_active_processes++;
        }
    }

    /* Wait for the child processes to exit */
    while (num_active_processes > 0)
    {
        rc = waitid (P_ALL, 0, &info, WEXITED);
        if (rc == 0)
        {
            for (process_index = 0; process_index < NUM_PROCESSES; process_index++)
            {
                test_process_t *const process = &test_processes[process_index];

                if (!process->reaped && (info.si_pid == process->pid))
                {
                    process->reaped = true;
                    num_active_processes--;
                }
            }
        }
    }

    return EXIT_SUCCESS;
}
