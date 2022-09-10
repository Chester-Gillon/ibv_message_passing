/*
 * @file bind_local.c
 * @date 4 Sep 2022
 * @author Chester Gillon
 * @details
 *   To investigate https://stackoverflow.com/questions/23907095/address-already-in-use-but-nothing-in-netstat-or-lsof
 *   and checking which ports iwpmd has reserved.
 */

#define _GNU_SOURCE /* Avoids the need for a cast in the address parameter to bind() */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>
#include <stdio.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>


int main (int argc, char *argv[])
{
    const char *const optstring = "-p:-4:-6:lu";
    int option;
    char junk;
    uint16_t port_to_bind = 0;
    bool listen_on_port = false;
    bool use_udp = false;
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
            {
                const struct addrinfo hints_ipv4 =
                {
                    .ai_family = AF_INET,
                    .ai_socktype = SOCK_STREAM,
                    .ai_protocol = 0,
                    .ai_flags = 0
                };
                struct addrinfo *res = NULL;

                rc = getaddrinfo (optarg, NULL, &hints_ipv4, &res);
                if (rc != 0)
                {
                    fprintf (stderr, "Error: %s does not resolve to a local IPv4 address : %s\n", optarg, gai_strerror (rc));
                    exit (EXIT_FAILURE);
                }
                memcpy (local_addr_in, res->ai_addr, res->ai_addrlen);
            }
            break;

        case '6':
            {
                const struct addrinfo hints_ipv6 =
                {
                    .ai_family = AF_INET6,
                    .ai_socktype = SOCK_STREAM,
                    .ai_protocol = 0,
                    .ai_flags = 0
                };
                struct addrinfo *res = NULL;

                rc = getaddrinfo (optarg, NULL, &hints_ipv6, &res);
                if (rc != 0)
                {
                    fprintf (stderr, "Error: %s does not resolve to a local IPv6 address : %s\n", optarg, gai_strerror (rc));
                    exit (EXIT_FAILURE);
                }
                memcpy (local_addr_in6, res->ai_addr, res->ai_addrlen);
            }
            break;

        case 'l':
            listen_on_port = true;
            break;

        case 'u':
            use_udp = true;
            break;

        case '?':
        default:
            printf ("Usage %s [-p <port>] [-4 <IPv4_local_addr>] [-6 <IPv6_local_addr>] [-l] [-u]\n", argv[0]);
            printf ("-l optionally specifies to listen on the port\n");
            printf ("-u specifies to create a SOCK_DGRAM (UDP) rather than SOCK_STREAM (TCP) socket\n");
            exit (EXIT_FAILURE);
            break;
        }

        option = getopt (argc, argv, optstring);
    }

    if (listen_on_port && use_udp)
    {
        printf ("Error: -l and -u can't be used together\n");
        exit (EXIT_FAILURE);
    }

    /* If no local address was specified on the command line, default to a IPv6 socket.
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
    socket_fd = socket ((local_addr.ss_family == AF_INET6) ? PF_INET6 : PF_INET, use_udp ? SOCK_DGRAM : SOCK_STREAM, 0);
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

        printf ("fd %d bound to %s scope-id %" PRIu32 " port %u\n", socket_fd, addr_string, local_addr_in6->sin6_scope_id, ntohs (local_addr_in6->sin6_port));
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

    /* Listen on port if requested */
    char prompt_string[40];
    if (listen_on_port)
    {
        printf ("Press enter to listen on port\n");
        fgets (prompt_string, sizeof (prompt_string), stdin);

        rc = listen (socket_fd, 1);
        if (rc != 0)
        {
            perror ("listen()");
            exit (EXIT_FAILURE);
        }
    }

    printf ("Press return to exit");
    fgets (prompt_string, sizeof (prompt_string), stdin);

    close (socket_fd);

    return EXIT_SUCCESS;
}
