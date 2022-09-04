/*
 * @file bind_local.c
 * @date 4 Sep 2022
 * @author Chester Gillon
 *
 * To investigate https://stackoverflow.com/questions/23907095/address-already-in-use-but-nothing-in-netstat-or-lsof
 * and checking which ports iwpmd has reserved.
 */

#define _GNU_SOURCE /* Avoids the need for a cast in the address parameter to bind() */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <inttypes.h>
#include <stdio.h>

#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>


int main (int argc, char *argv[])
{
    const char *const optstring = "-p:-4:-6:";
    int option;
    char junk;
    uint16_t port_to_bind = 0;
    struct sockaddr_storage local_addr =
    {
        .ss_family = AF_UNSPEC
    };
    struct sockaddr_in *const local_addr_in = (struct sockaddr_in *) &local_addr;
    struct sockaddr_in6 *const local_addr_in6 = (struct sockaddr_in6 *) &local_addr;
    int rc;
    int socket_fd;

    /* Process the command line arguments */
    option = getopt (argc, argv, optstring);
    while (option != -1)
    {
        switch (option)
        {
        case 'p':
            if (sscanf (optarg, "%" SCNu16 "%c", &port_to_bind, &junk) != 1)
            {
                fprintf (stderr, "Error: %s is not a valid port number\n", optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case '4':
            local_addr.ss_family = AF_INET;
            rc = inet_pton (AF_INET, optarg, &local_addr_in->sin_addr);
            if (rc != 1)
            {
                fprintf (stderr, "Error: %s is not a valid IPv4 address\n", optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case '6':
            local_addr.ss_family = AF_INET6;
            rc = inet_pton (AF_INET6, optarg, &local_addr_in6->sin6_addr);
            if (rc != 1)
            {
                fprintf (stderr, "Error: %s is not a valid IPv6 address\n", optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case '?':
        default:
            printf ("Usage %s [-p <port>] [-4 <IPv4_local_addr>] [-6 <IPv6_local_addr>]\n", argv[0]);
            break;
        }

        option = getopt (argc, argv, optstring);
    }

    /* If no IP address was specified on the command line, default to a IPv6 socket.
     * Depending upon the value of the IPV6_V6ONLY socket option may allow a IPv4 connection. */
    if (local_addr.ss_family == AF_UNSPEC)
    {
        local_addr.ss_family = AF_INET6;
    }

    /* Set any port number specified on the command line */
    if (local_addr.ss_family == AF_INET6)
    {
        local_addr_in6->sin6_port = htons (port_to_bind);
    }
    else
    {
        local_addr_in->sin_port = htons (port_to_bind);
    }

    /* Create socket of appropriate family */
    socket_fd = socket ((local_addr.ss_family == AF_INET6) ? PF_INET6 : PF_INET, SOCK_STREAM, 0);
    if (socket_fd == -1)
    {
        perror ("socket()");
        exit (EXIT_FAILURE);
    }

    /* Bind the local address */
    if (local_addr.ss_family == AF_INET6)
    {
        rc = bind (socket_fd, local_addr_in6, sizeof (*local_addr_in6));
    }
    else
    {
        rc = bind (socket_fd, local_addr_in, sizeof (*local_addr_in));
    }
    if (rc != 0)
    {
        perror ("bind()");
        exit (EXIT_FAILURE);
    }

    /* Display the address the socket has been bound to */
    memset (&local_addr, 0, sizeof (local_addr));
    socklen_t addr_len = sizeof (local_addr);
    rc = getsockname (socket_fd, (struct sockaddr *) &local_addr, &addr_len);
    if (rc != 0)
    {
        perror ("getsockname()");
        exit (EXIT_FAILURE);
    }

    char addr_string[INET6_ADDRSTRLEN];
    const char *ntop_status;
    if (local_addr.ss_family == AF_INET6)
    {
        ntop_status = inet_ntop (AF_INET6, &local_addr_in6->sin6_addr, addr_string, sizeof (addr_string));
        if (ntop_status == NULL)
        {
            perror ("inet_ntop(AF_INET6)");
            exit (EXIT_FAILURE);
        }

        printf ("fd %d bound to %s port %u\n", socket_fd, addr_string, ntohs (local_addr_in6->sin6_port));
    }
    else
    {
        ntop_status = inet_ntop (AF_INET, &local_addr_in->sin_addr, addr_string, sizeof (addr_string));
        if (ntop_status == NULL)
        {
            perror ("inet_ntop(AF_INET)");
            exit (EXIT_FAILURE);
        }

        printf ("fd %d bound to %s port %u\n", socket_fd, addr_string, ntohs (local_addr_in->sin_port));
    }

    char exit_string[40];
    printf ("Press return to exit");
    fgets (exit_string, sizeof (exit_string), stdin);

    close (socket_fd);

    return EXIT_SUCCESS;
}
