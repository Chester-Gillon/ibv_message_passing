/*
 * @file ibv_monitor_port_state.c
 * @date 14 Apr 2019
 * @author Chester Gillon
 * @brief Use asynchronous events to monitor changes to the port state for local Infiniband local devices
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

#include <errno.h>
#include <time.h>
#include <poll.h>
#include <signal.h>

#include <infiniband/verbs.h>


/* Used to size arrays */
#define MAX_DEVICES 16


/* Set true from a signal handler to request that the program exits */
static volatile bool exit_requested = false;


static void stop_test (const int sig)
{
    exit_requested = true;
}


/**
 * @brief Display the current time, and the time difference since the previous call to this function
 */
static void display_timestamp (void)
{
    static bool first_call = true;
    static struct timespec previous;
    struct timespec now;
    struct tm broken_down_time;
    int rc;
    const int64_t nsecs_per_sec = 1000000000L;

    rc = clock_gettime (CLOCK_REALTIME, &now);
    if (rc != 0)
    {
        perror ("clock_gettime failed");
        exit (EXIT_FAILURE);
    }
    localtime_r (&now.tv_sec, &broken_down_time);

    printf ("%02d:%02d:%02d.%06" PRIi64, broken_down_time.tm_hour, broken_down_time.tm_min, broken_down_time.tm_sec,
            now.tv_nsec / 1000);

    if (first_call)
    {
        first_call = false;
    }
    else
    {
        const int64_t previous_ns = (previous.tv_sec * nsecs_per_sec) + previous.tv_nsec;
        const int64_t now_ns = (now.tv_sec * nsecs_per_sec) + now.tv_nsec;
        const double time_diff_secs = (now_ns - previous_ns) / 1E9;

        printf (" (+%.6f)", time_diff_secs);
    }
    printf (" ");

    previous = now;
}


int main (int argc, char *argv[])
{
    int device_index;
    int rc;
    struct ibv_port_attr port_attributes;
    struct pollfd poll_fds[MAX_DEVICES] = {0};
    struct ibv_async_event event;

    /* Get the local Infiniband devices */
    int num_devices;
    struct ibv_device **const device_list = ibv_get_device_list (&num_devices);
    if ((num_devices == 0) || (device_list == NULL))
    {
        fprintf (stderr, "No Infiniband devices found\n");
        exit (EXIT_FAILURE);
    }

    /* Open the context for all local Infiniband devices, and display the initial port state */
    struct ibv_context *device_contexts[MAX_DEVICES] = {NULL};
    struct ibv_device_attr device_attributes[MAX_DEVICES] = {0};

    for (device_index = 0; device_index < num_devices; device_index++)
    {
        device_contexts[device_index] = ibv_open_device (device_list[device_index]);
        if (device_contexts[device_index] == NULL)
        {
            perror ("ibv_open_device failed");
            exit (EXIT_FAILURE);
        }

        rc = ibv_query_device (device_contexts[device_index], &device_attributes[device_index]);
        if (rc != 0)
        {
            perror ("ibv_query_device failed");
            exit (EXIT_FAILURE);
        }

        poll_fds[device_index].fd = device_contexts[device_index]->async_fd;
        poll_fds[device_index].events = POLLIN;

        for (int port_num = 1; port_num <= device_attributes[device_index].phys_port_cnt; port_num++)
        {
            rc = ibv_query_port (device_contexts[device_index], port_num, &port_attributes);
            if (rc != 0)
            {
                perror ("ibv_query_port failed");
                exit (EXIT_FAILURE);
            }
            display_timestamp ();
            printf ("%s port %d initial state=%s",
                    ibv_get_device_name (device_list[device_index]), port_num, ibv_port_state_str (port_attributes.state));
            if (port_attributes.state == IBV_PORT_ACTIVE)
            {
                printf (" LID=%u", port_attributes.lid);
            }
            printf ("\n");
        }
    }
    /* Install a signal handler to allow a request to stop transmission */
    struct sigaction action;

    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_test;
    action.sa_flags = 0;
    rc = sigaction (SIGINT, &action, NULL);
    if (rc != 0)
    {
        perror ("sigaction failed");
        exit (EXIT_FAILURE);
    }

    /* Monitor Infiniband device asynchronous events until requested to exit */
    while (!exit_requested)
    {
        errno = 0;
        rc = poll (poll_fds, num_devices, -1);
        if (rc > 0)
        {
            for (device_index = 0; device_index < num_devices; device_index++)
            {
                if (poll_fds[device_index].revents & POLLIN)
                {
                    rc = ibv_get_async_event (device_contexts[device_index], &event);
                    if (rc != 0)
                    {
                        perror ("ibv_get_async_event failed");
                        exit (EXIT_FAILURE);
                    }

                    display_timestamp ();
                    printf ("%s ", ibv_get_device_name (device_list[device_index]));

                    switch (event.event_type)
                    {
                    case IBV_EVENT_PORT_ACTIVE:
                        printf ("port %d ACTIVE\n", event.element.port_num);
                        break;

                    case IBV_EVENT_LID_CHANGE:
                        printf ("port %d LID_CHANGE\n", event.element.port_num);
                        break;

                    case IBV_EVENT_PORT_ERR:
                        printf ("port %d ERROR\n", event.element.port_num);
                        break;

                    case IBV_EVENT_CLIENT_REREGISTER:
                        printf ("port %d CLIENT_REREGISTER\n", event.element.port_num);
                        break;

                    default:
                        printf ("event_type=%d\n", event.event_type);
                        break;
                    }

                    ibv_ack_async_event (&event);
                }
            }
        }
        else if (errno != EINTR)
        {
            perror ("poll failed");
            exit (EXIT_FAILURE);
        }
    }

    /* Free resources */
    for (device_index = 0; device_index < num_devices; device_index++)
    {
        rc = ibv_close_device (device_contexts[device_index]);
        if (rc != 0)
        {
            perror ("ibv_close_device failed");
            exit (EXIT_FAILURE);
        }
    }
    ibv_free_device_list (device_list);

    display_timestamp ();
    printf ("Exit requested\n");

    return EXIT_SUCCESS;
}
