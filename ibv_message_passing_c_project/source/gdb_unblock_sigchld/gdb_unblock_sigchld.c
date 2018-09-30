/*
 * @file gdb_unblock_sigchld.c
 * @date 7 Jul 2018
 * @author Chester Gillon
 * @brief Wrapper for starting gdb in Eclipse with GNATbench installed ensuring that SIGCHLD is not blocked
 * @details The GNATbench plug-in sometimes causes GDB to be started with SIGCHLD blocked, which results in the debug session
 *          hanging as GDB fails to detect when the child process being debugged traces.
 *          This wrapper, which is set as the "GDB debugger" command in the Eclipse debug configuration, changes the blocked
 *          signals before starting gdb to work-around the problem.
 */

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <syslog.h>

int main (int argc, char *argv[])
{
    char **modified_argv;
    sigset_t original_masked_signals;
    sigset_t modified_masked_signals;
    int rc;

    /* Create a copy of the argv array for gdb, setting the first entry to the GDB name */
    modified_argv = calloc (argc + 1, sizeof (char *));
    memcpy (modified_argv, argv, argc * sizeof (char *));
    modified_argv[0] = strdup ("gdb");

    /* If SIGCHLD is blocked, then replaced the blocked signals with SIQQUIT, which is the initial value in GDB when can
     * successfully start a debug session.
     *
     * Writes a syslog message to record that have taken action. */
    rc = pthread_sigmask (SIG_SETMASK, NULL, &original_masked_signals);
    if (rc == 0)
    {
        rc = sigismember (&original_masked_signals, SIGCHLD);
        if (rc == 1)
        {
            rc = sigemptyset (&modified_masked_signals);
            if (rc == 0)
            {
                rc = sigaddset (&modified_masked_signals, SIGQUIT);
                if (rc == 0)
                {
                    rc = pthread_sigmask (SIG_SETMASK, &modified_masked_signals, NULL);
                    if (rc == 0)
                    {
                        syslog (LOG_INFO, "Changing SigBlk %016lx -> %016lx",
                                original_masked_signals.__val[0], modified_masked_signals.__val[0]);
                    }
                }
            }
        }
    }

    /* Replace the image with gdb */
    rc = execvp ("gdb", modified_argv);

    return rc;
}
