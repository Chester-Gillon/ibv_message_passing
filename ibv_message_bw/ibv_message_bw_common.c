/*
 * @file ibv_message_bw_common.c
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains functions common the message transmitter and receiver for the ibv_message_bw program
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

#include <infiniband/verbs.h>

#include "ibv_message_bw_interface.h"

/**
 * @brief Perform a page size aligned allocation
 * @param[in] size The size of the allocated
 * @return Returns a pointer to page aligned allocation
 */
void *page_aligned_alloc (const size_t size)
{
    int rc;
    const size_t page_size = sysconf (_SC_PAGE_SIZE);
    void *buffer;

    rc = posix_memalign (&buffer, page_size, size);
    if (rc != 0)
    {
        errno = rc;
        perror ("posix_memalign failed");
        exit (EXIT_FAILURE);
    }

    return buffer;
}

/**
 * @brief Perform a page size aligned allocation, and zero the allocated memory
 * @param[in] nmemb Number of elements in the array to allocate
 * @param[in] size The size of each element in the array to allocate
 * @return Return a pointer to the zeroed page aligned allocation
 */
void *page_aligned_calloc (const size_t nmemb, const size_t size)
{
    void *const buffer = page_aligned_alloc (nmemb * size);

    memset (buffer, 0, size);

    return buffer;
}

/**
 * @brief Perform a cache line size aligned allocation
 * @param[in] size The size of the allocated
 * @return Returns a pointer to page aligned allocation
 */
void *cache_line_aligned_alloc (const size_t size)
{
    int rc;
    void *buffer;

    rc = posix_memalign (&buffer, CACHE_LINE_SIZE_BYTES, size);
    if (rc != 0)
    {
        errno = rc;
        perror ("posix_memalign failed");
        exit (EXIT_FAILURE);
    }

    return buffer;
}

/**
 * @brief Perform a cache line size aligned allocation, and zero the allocated memory
 * @param[in] nmemb Number of elements in the array to allocate
 * @param[in] size The size of each element in the array to allocate
 * @return Return a pointer to the zeroed page aligned allocation
 */
void *cache_line_aligned_calloc (const size_t nmemb, const size_t size)
{
    void *const buffer = cache_line_aligned_alloc (nmemb * size);

    memset (buffer, 0, size);

    return buffer;
}

/**
 * @brief Abort the program if an assertion check fails
 * @param[in] assertion If not true a programming error has been detected
 * @param[in] message Displayed if the assertion check fails
 */
void check_assert (const bool assertion, const char *message)
{
    if (!assertion)
    {
        printf ("Assertion failed: %s\n", message);
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Open an Infiniband device and port to be as the endpoint for transmitting or receiving messages
 * @param[out] endpoint The opened endpoint
 * @param[in] path_def Defines which Infiniband device and port to open
 */
void open_ib_port_endpoint (ib_port_endpoint *const endpoint, const communication_path_definition *const path_def)
{
    int rc;
    int device_index;

    /* Get the available Infiniband devices */
    endpoint->device_list = ibv_get_device_list (&endpoint->num_devices);
    if (endpoint->device_list == NULL)
    {
        perror ("ibv_get_device_list failed");
        exit (EXIT_FAILURE);
    }
    if (endpoint->num_devices == 0)
    {
        fprintf (stderr, "No Infiniband devices found\n");
        exit (EXIT_FAILURE);
    }

    /* Find the Infiniband device to be used for the communication path */
    endpoint->selected_device = NULL;
    for (device_index = 0; (endpoint->selected_device == NULL) && (device_index < endpoint->num_devices); device_index++)
    {
        if (strcmp (path_def->ib_device, ibv_get_device_name (endpoint->device_list[device_index])) == 0)
        {
            endpoint->selected_device = endpoint->device_list[device_index];
        }
    }

    if (endpoint->selected_device == NULL)
    {
        fprintf (stderr, "Infiniband device %s does not exist. Available devices are", path_def->ib_device);
        for (device_index = 0; device_index < endpoint->num_devices; device_index++)
        {
            fprintf (stderr, " %s", ibv_get_device_name (endpoint->device_list[device_index]));
        }
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }

    /* Open the Infiniband device and get its attributes */
    endpoint->device_context = ibv_open_device (endpoint->selected_device);
    if (endpoint->device_context == NULL)
    {
        fprintf (stderr, "ibv_open_device failed\n");
        exit (EXIT_FAILURE);
    }

    rc = ibv_query_device (endpoint->device_context, &endpoint->device_attributes);
    if (rc != 0)
    {
        perror ("ibv_query_device failed");
        exit (EXIT_FAILURE);
    }

    /* Check the Infiniband port number is valid, and obtain the port attributes */
    if ((path_def->port_num < 1) || (path_def->port_num > endpoint->device_attributes.phys_port_cnt))
    {
        fprintf (stderr, "Infiniband port %u is out of range for device %s, valid ports are 1..%u\n",
                 path_def->port_num, path_def->ib_device, endpoint->device_attributes.phys_port_cnt);
        exit (EXIT_FAILURE);
    }
    rc = ibv_query_port (endpoint->device_context, path_def->port_num, &endpoint->port_attributes);
    if (rc != 0)
    {
        perror ("ibv_query_port failed");
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Close an Infiniband device port endpoint, freeing the resources allocated by open_ib_port_endpoint()
 * @param[in,out] endpoint The endpoint to close
 */
void close_ib_port_endpoint (ib_port_endpoint *const endpoint)
{
    int rc;

    rc = ibv_close_device (endpoint->device_context);
    check_assert (rc == 0, "ibv_close_device");
    endpoint->device_context = NULL;

    ibv_free_device_list (endpoint->device_list);
    check_assert (rc == 0, "ibv_free_device_list");
    endpoint->num_devices = 0;
}
