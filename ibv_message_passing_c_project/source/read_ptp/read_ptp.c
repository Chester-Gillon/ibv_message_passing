/*
 * @file read_ptp.c
 * @date 22 July 2018
 * @author Chester Gillon
 * @brief Test access a PTP clock as a POSIX clock.
 */
/*
 * read_ptp.c
 *
 *  Created on: 21 Jul 2018
 *      Author: mr_halfword
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#include <phc.h>


static void display_clock_info (const clockid_t id, const char *const clock_name)
{
    int rc;
    struct timespec clock_res;
    struct timespec now;


    rc = clock_getres (id, &clock_res);
    if (rc != 0)
    {
        perror ("clock_getres");
        exit (EXIT_FAILURE);
    }
    printf ("%s : res tv_sec=%ld tv_nsec=%ld\n", clock_name, clock_res.tv_sec, clock_res.tv_nsec);

    rc = clock_gettime (id, &now);
    if (rc != 0)
    {
        perror ("clock_getres");
        exit (EXIT_FAILURE);
    }
    printf ("%s : now tv_sec=%ld tv_nsec=%ld\n", clock_name, now.tv_sec, now.tv_nsec);
}


int main (int argc, char *argv[])
{
    clockid_t ptp_id;

    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s </dev/ptpN>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    char *const ptp_dev_name = argv[1];
    ptp_id = phc_open (ptp_dev_name);
    if (ptp_id == CLOCK_INVALID)
    {
        perror ("phc_open failed");
        exit (EXIT_FAILURE);
    }

    printf ("phc_has_pps=%d\n", phc_has_pps (ptp_id));
    printf ("phc_max_adj=%d\n", phc_max_adj (ptp_id));

    display_clock_info (CLOCK_REALTIME, "CLOCK_REALTIME");
    display_clock_info (CLOCK_MONOTONIC, "CLOCK_MONOTONIC");
    display_clock_info (ptp_id, ptp_dev_name);

    phc_close (ptp_id);

    return EXIT_SUCCESS;
}
