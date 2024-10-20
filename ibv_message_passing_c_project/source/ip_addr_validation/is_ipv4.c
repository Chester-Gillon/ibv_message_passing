/*
 * @file is_ipv4.c
 * @date 20 Oct 2024
 * @author Chester Gillon
 * @brief Use getaddrinfo() to validate in the input arguments are numeric IP addresses.
 * @details This doesn't accept an trailing CIDR (netmask) prefix
 */

#include <stdlib.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int main (int argc, char *argv[])
{
    const struct addrinfo hints_ipv4 =
    {
        .ai_family = AF_INET,
        .ai_socktype = SOCK_STREAM,
        .ai_protocol = 0,
        .ai_flags = AI_NUMERICHOST
    };
    int rc;

    for (int addr_index = 1; addr_index < argc; addr_index++)
    {
        const char *const addr_string = argv[addr_index];
        struct addrinfo *res = NULL;

        rc = getaddrinfo (addr_string, NULL, &hints_ipv4, &res);
        if (rc == 0)
        {
            printf ("yes, %s is allowed\n", addr_string);
        }
    }

    return EXIT_SUCCESS;
}
