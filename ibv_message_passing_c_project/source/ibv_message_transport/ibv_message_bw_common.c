/*
 * @file ibv_message_bw_common.c
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains functions common the message transmitter and receiver for the ibv_message_bw program
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <infiniband/verbs.h>
#include <slp.h>

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
 * @brief Open an Infiniband device and port to be used as the endpoint for transmitting or receiving messages
 * @param[out] endpoint The opened endpoint
 * @param[in] path_def Defines which Infiniband device and port to open
 * @param[in] is_tx_end Determines if the calling thread is the transmit or receive end of the communication path
 */
void open_ib_port_endpoint (ib_port_endpoint *const endpoint, const communication_path_definition *const path_def,
                            const bool is_tx_end)
{
    const char *const ib_device = is_tx_end ? path_def->source_ib_device : path_def->destination_ib_device;
    const uint8_t port_num = is_tx_end ? path_def->source_port_num : path_def->destination_port_num;
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
        if (strcmp (ib_device, ibv_get_device_name (endpoint->device_list[device_index])) == 0)
        {
            endpoint->selected_device = endpoint->device_list[device_index];
        }
    }

    if (endpoint->selected_device == NULL)
    {
        fprintf (stderr, "Infiniband device %s does not exist. Available devices are", ib_device);
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

    /* Create a protection domain */
    endpoint->device_pd = ibv_alloc_pd (endpoint->device_context);
    if (endpoint->device_pd == NULL)
    {
        perror ("ibv_alloc_pd failed");
        exit (EXIT_FAILURE);
    }

    /* Check the Infiniband port number is valid, and obtain the port attributes */
    if ((port_num < 1) || (port_num > endpoint->device_attributes.phys_port_cnt))
    {
        fprintf (stderr, "Infiniband port %u is out of range for device %s, valid ports are 1..%u\n",
                 port_num, ib_device, endpoint->device_attributes.phys_port_cnt);
        exit (EXIT_FAILURE);
    }
    rc = ibv_query_port (endpoint->device_context, port_num, &endpoint->port_attributes);
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

    rc = ibv_dealloc_pd (endpoint->device_pd);
    check_assert (rc == 0, "ibv_dealloc_pd");
    endpoint->device_pd = NULL;

    rc = ibv_close_device (endpoint->device_context);
    check_assert (rc == 0, "ibv_close_device");
    endpoint->device_context = NULL;

    ibv_free_device_list (endpoint->device_list);
    check_assert (rc == 0, "ibv_free_device_list");
    endpoint->num_devices = 0;
}

/**
 * @brief Initialise the SLP connection to be used exchange Queue Pair information for a communication path
 * @param[out] slp_connection The connection to initialise
 * @param[in] is_tx_end Determines if the calling thread is the transmit or receive end of the communication path
 * @param[in] path_def The definition of the communication path, used to build a unique service URL
 */
void intialise_slp_connection (communication_path_slp_connection *const slp_connection, const bool is_tx_end,
                               const communication_path_definition *const path_def)
{
    SLPError slp_status;
    int rc;
    char hostname[PATH_MAX];
    const char *const local_end  = is_tx_end ? "tx" : "rx";
    const char *const remote_end = is_tx_end ? "rx" : "tx";

    slp_connection->remote_attributes_obtained = false;
    rc = gethostname (hostname, sizeof (hostname));
    check_assert (rc == 0, "gethostname");
    sprintf (slp_connection->local_service_url, "%s://%s/name=%s_%d_%d_%d",
             SLP_SERVICE_NAME, hostname, local_end,
             path_def->source_node, path_def->destination_node, path_def->instance);
    sprintf (slp_connection->remote_service_name, "/name=%s_%d_%d_%d", remote_end,
             path_def->source_node, path_def->destination_node, path_def->instance);
    strcpy (slp_connection->remote_service_url, "");

    slp_status = SLPOpen (NULL, SLP_FALSE, &slp_connection->handle);
    check_assert (slp_status == SLP_OK, "SLPOpen");
}

/**
 * @brief Callback for SLPReg to store the status of a service URL (de-)registration
 * @param[in] handle Not used
 * @param[in] error_code The status for the SLP (de-)registration
 * @param[out] cookie Where to store the (de-)registration status
 */
static void slp_reg_callback (const SLPHandle handle, const SLPError error_code, void *const cookie)
{
    *((SLPError *) cookie) = error_code;
}

/**
 * @brief Publish a local memory buffer with SLP, to allow the attributes to be retrieved by the remote endpoint
 * @param[in] slp_connection The SLP connection to use for the registration, and the local memory buffer attributes
 */
static void publish_memory_buffer_with_slp (const communication_path_slp_connection *const slp_connection)
{
    char attributes_text[SLP_ATTRIBUTES_MAX_LEN];
    SLPError slp_status;
    SLPError registration_status;

    sprintf (attributes_text, "(size=%lu,rkey=%u,addr=0x%lx,lid=%u,psn=%u,qp_num=%u,qp_rtr=%d)",
             slp_connection->local_attributes.size, slp_connection->local_attributes.rkey, slp_connection->local_attributes.addr,
             slp_connection->local_attributes.lid, slp_connection->local_attributes.psn, slp_connection->local_attributes.qp_num,
             slp_connection->local_attributes.qp_ready_to_receive);
    slp_status = SLPReg (slp_connection->handle, slp_connection->local_service_url, SLP_LIFETIME_MAXIMUM, NULL, attributes_text,
            SLP_TRUE, slp_reg_callback, &registration_status);
    check_assert (slp_status == SLP_OK, "SLPReg");
    check_assert (registration_status == SLP_OK, "registration_status");
}

/**
 * @brief Register a memory for transmitting or receiving message with SLP, with attributes which allow the remote end to attach
 * @param[in,out] slp_connection The SLP connection to use for the service URL registration
 * @param[in] endpoint The Infiniband port for the local endpoint, used to publish the LID
 * @param[in] psn The local initial packet sequence number to publish
 * @param[in] mr The local memory region to publish
 * @param[in] qp The local Queue Pair, to publish the Queue Pair Number
 */
void register_memory_buffer_with_slp (communication_path_slp_connection *const slp_connection,
                                      const ib_port_endpoint *const endpoint, const uint32_t psn,
                                      const struct ibv_mr *const mr, const struct ibv_qp *const qp)
{
    slp_connection->local_attributes.size = mr->length;
    slp_connection->local_attributes.rkey = mr->rkey;
    slp_connection->local_attributes.addr = (uintptr_t) mr->addr;
    slp_connection->local_attributes.lid = endpoint->port_attributes.lid;
    slp_connection->local_attributes.psn = psn;
    slp_connection->local_attributes.qp_num = qp->qp_num;
    slp_connection->local_attributes.qp_ready_to_receive = false;
    publish_memory_buffer_with_slp (slp_connection);
}

/**
 * @details Called after the Queue-Pair for a local memory buffer is ready-to-receive, to publish an update to the SLP attributes
 *          for the memory buffer.
 * @param[in,out] The SLP connection to use for the registration, and the local memory buffer attributes
 */
void report_local_memory_buffer_rtr_with_slp (communication_path_slp_connection *const slp_connection)
{
    slp_connection->local_attributes.qp_ready_to_receive = true;
    publish_memory_buffer_with_slp (slp_connection);
}

/**
 * @details Callback for SLPFindSrvs to store the full service URL for a service which matches the name of the remote endpoint
 *          for a communication path.
 * @param[in] handle Not used
 * @param[in] service_url The service URL to check
 * @param[in] lifetime Not used
 * @param[in] error_code Indicates the status of retrieving the service URL
 * @param[in,out] cookie Points at the SLP connection with the required endpoint
 * @return Returns SLP_TRUE if more data is required
 */
static SLPBoolean slp_service_url_callback (const SLPHandle handle, const char *service_url, const unsigned short lifetime,
                                            const SLPError error_code, void *const cookie)
{
    communication_path_slp_connection *const slp_connection = (communication_path_slp_connection *) cookie;
    const char *match;

    if ((error_code == SLP_OK) && (strlen (slp_connection->remote_service_url) == 0))
    {
        match = strstr (service_url, slp_connection->remote_service_name);
        if ((match != NULL) && (strlen (match) == strlen (slp_connection->remote_service_name)))
        {
            strcpy (slp_connection->remote_service_url, service_url);
        }
    }

    /* Only request further data if haven't obtained the required remote service URL */
    return (strlen (slp_connection->remote_service_url) == 0) ? SLP_TRUE : SLP_FALSE;
}

/**
 * @brief Callback for SLPFindAttrs to extract the attributes of a remote memory region from the SLP attributes string
 * @param[in] handle Not used
 * @param[in] attributes The name-value SLP attributes string to parse
 * @param[in] error_code Indicates the status of retrieving the service URL attributes
 * @return Returns SLP_TRUE if more data is required
 */
static SLPBoolean slp_service_attributes_callback (const SLPHandle handle, const char *attributes,
                                                   const SLPError error_code, void *const cookie)
{
    communication_path_slp_connection *const slp_connection = (communication_path_slp_connection *) cookie;

    if ((error_code == SLP_OK) && (!slp_connection->remote_attributes_obtained))
    {
        const int num_items = sscanf (attributes, "(size=%lu,rkey=%u,addr=0x%lx,lid=%hu,psn=%u,qp_num=%u,qp_rtr=%d)",
                &slp_connection->remote_attributes.size, &slp_connection->remote_attributes.rkey,
                &slp_connection->remote_attributes.addr, &slp_connection->remote_attributes.lid,
                &slp_connection->remote_attributes.psn, &slp_connection->remote_attributes.qp_num,
                &slp_connection->remote_attributes.qp_ready_to_receive);

        slp_connection->remote_attributes_obtained = num_items == 7;
    }

    /* Only request further data if haven't extracted the required attributes */
    return slp_connection->remote_attributes_obtained ? SLP_FALSE : SLP_TRUE;
}

/**
 * @details Called after a SLP operation has failed to obtain the required remote memory buffer attributes,
 *          to insert a hold off delay to avoid flooding the SLP daemon with requests
 * @param[in] start_time CLOCK_MONOTONIC start time for the SLP operation, used to set the time to delay to
 */
static void slp_retry_holdoff (const struct timespec *const start_time)
{
    const uint64_t nsecs_per_sec = 1000000000;
    const uint64_t holdoff_delay_ns = 100000000; /* 100 milliseconds */
    struct timespec holdoff_time;
    int rc;

    holdoff_time = *start_time;
    holdoff_time.tv_nsec += holdoff_delay_ns;
    if (holdoff_time.tv_nsec >= nsecs_per_sec)
    {
        holdoff_time.tv_sec++;
        holdoff_time.tv_nsec -= nsecs_per_sec;
    }
    rc = clock_nanosleep (CLOCK_MONOTONIC, TIMER_ABSTIME, &holdoff_time, NULL);
    check_assert (rc == 0, "clock_nanosleep");
}

/**
 * @brief Retrive the attributes for a remote memory buffer using SLP
 * @param[in,out] slp_connection The connection to retrieve the remote attributes for
 */
static void get_remote_buffer_attributes_from_slp (communication_path_slp_connection *const slp_connection)
{
    SLPError slp_status;

    slp_connection->remote_attributes_obtained = false;
    while (!slp_connection->remote_attributes_obtained)
    {
        if (strlen (slp_connection->remote_service_url) == 0)
        {
            /* Attempt to find the service URL for the remote memory buffer */
            slp_status = SLPFindSrvs (slp_connection->handle, SLP_SERVICE_NAME, NULL, NULL,
                    slp_service_url_callback, slp_connection);
            check_assert (slp_status == SLP_OK, "SLPFindSrvs");
        }

        if (strlen (slp_connection->remote_service_url) > 0)
        {
            /* The service URL is available, so attempt to retrieve the attributes */
            slp_connection->remote_attributes_obtained = false;
            slp_status = SLPFindAttrs (slp_connection->handle, slp_connection->remote_service_url, "", "",
                    slp_service_attributes_callback, slp_connection);
            check_assert (slp_status == SLP_OK, "SLPFindAttrs");
        }
    }
}

/**
 * @brief Use SLP to retrieve the attributes of the remote memory buffer.
 * @details This is required to connect the Queue-Pairs between the local and remote ends of a communication path.
 *          Depending upon timing, this may sample the attributes before or after the remote memory buffer
 *          has transitioned to ready-to-receive
 * @param[in,out] slp_connection The connection to retrieve the remote attributes for
 */
void get_remote_memory_buffer_from_slp (communication_path_slp_connection *const slp_connection)
{
    struct timespec start_time;

    do
    {
        clock_gettime (CLOCK_MONOTONIC, &start_time);
        get_remote_buffer_attributes_from_slp (slp_connection);
        if (!slp_connection->remote_attributes_obtained)
        {
            slp_retry_holdoff (&start_time);
        }
    } while (!slp_connection->remote_attributes_obtained);
}

/**
 * @brief Use SLP to wait for the attributes of the remote memory buffer to indicate is ready-to-receive
 * @details When this function returns the Queue-Pair for the remote memory buffer is in a state to accept
 *          Infiniband writes.
 *
 *          Depending upon timing, the previous call to get_remote_memory_buffer_from_slp() may have already
 *          sampled the attributes of the remote memory buffer as ready-to-receive, in which case this function
 *          doesn't perform any SLP operations.
 * @param[in,out] slp_connection The connection to wait for the remote memory buffer to be ready-to-receive
 */
void await_remote_memory_buffer_rtr_from_slp (communication_path_slp_connection *const slp_connection)
{
    struct timespec start_time;

    while (!slp_connection->remote_attributes.qp_ready_to_receive)
    {
        clock_gettime (CLOCK_MONOTONIC, &start_time);
        get_remote_buffer_attributes_from_slp (slp_connection);
        if (!slp_connection->remote_attributes.qp_ready_to_receive)
        {
            slp_retry_holdoff (&start_time);
        }
    }
}

/**
 * @brief Close the SLP connection which has been used to exchange Queue Pair information for a communication path
 * @param[in,out] The connection to close
 */
void close_slp_connection (communication_path_slp_connection *const slp_connection)
{
    SLPError slp_status;
    SLPError deregistration_status;

    /* de-register local_service_url */
    slp_status = SLPDereg (slp_connection->handle, slp_connection->local_service_url, slp_reg_callback, &deregistration_status);
    if (slp_status == SLP_NETWORK_ERROR)
    {
        /* If the test has been running for a period of time, the slpd daemon have have closed the socket our process was using,
         * causing SLPDereg to fail with SLP_NETWORK_ERROR.
         * In which case close and re-open the handle before attempting to repeat the de-registration. */
        SLPClose (slp_connection->handle);
        slp_status = SLPOpen (NULL, SLP_FALSE, &slp_connection->handle);
        check_assert (slp_status == SLP_OK, "SLPOpen");
        slp_status = SLPDereg (slp_connection->handle, slp_connection->local_service_url,
                slp_reg_callback, &deregistration_status);
    }
    check_assert (slp_status == SLP_OK, "SLPDereg");
    check_assert (deregistration_status == SLP_OK, "deregistration_status");

    SLPClose (slp_connection->handle);
}

/**
 * @brief Align a size to be a multiple of a cache line
 * @param[in] The size to align
 * @return The aligned size
 */
size_t align_to_cache_line_size (const size_t size)
{
    return ((size + CACHE_LINE_SIZE_BYTES - 1) / CACHE_LINE_SIZE_BYTES) * CACHE_LINE_SIZE_BYTES;
}

/**
 * @brief Create a memory buffer to be used to transmit or receive messages from
 * @param[out] buffer The created memory buffer, which has been mapped into the virtual address space
 * @param[in] is_tx_end Determines if the calling thread is the transmit or receive end of the communication path,
 *                      for setting the pathname of any underlying memory mapped file.
 * @param[in] path_def The definition of the communication path used to set:
 *                     - The buffer size.
 *                     - The pathname of any underlying memory mapped file.
 */
void create_memory_buffer (memory_buffer *const buffer, const bool is_tx_end,
                           const communication_path_definition *const path_def)
{
    int rc;

    memset (buffer, 0, sizeof (memory_buffer));
    buffer->allocation_type = path_def->allocation_type;
    buffer->size = path_def->num_message_buffers *
                    (align_to_cache_line_size (sizeof (message_header)) +    /* Message header */
                     align_to_cache_line_size (path_def->max_message_size) + /* Message body */
                     align_to_cache_line_size (sizeof (uint32_t)));          /* Freed sequence number */

    switch (buffer->allocation_type)
    {
    case BUFFER_ALLOCATION_HEAP:
        /* Simply allocate the buffer from the heap */
        buffer->buffer = page_aligned_calloc (1, buffer->size);
        break;

    case BUFFER_ALLOCATION_SHARED_MEMORY:
        /* Create a POSIX shared memory file */
        sprintf (buffer->pathname, "/ibv_message_bw_%s_%d_%d_%d", is_tx_end ? "tx" : "rx",
                 path_def->source_node, path_def->destination_node, path_def->instance);
        buffer->fd = shm_open (buffer->pathname, O_RDWR | O_CREAT | O_EXCL, S_IRWXU | S_IRWXG | S_IRWXO);
        if (buffer->fd < 0)
        {
            perror ("shm_open(O_CREAT) failed");
            exit (EXIT_FAILURE);
        }

        rc = posix_fallocate (buffer->fd, 0, buffer->size);
        check_assert (rc == 0, "posix_fallocate");
        rc = fsync (buffer->fd);
        check_assert (rc == 0, "fsync");
        rc = close (buffer->fd);

        /* Map the POSIX shared memory file into the virtual address space */
        buffer->fd = shm_open (buffer->pathname, O_RDWR, 0);
        if (buffer->fd < 0)
        {
            perror ("shm_open failed");
            exit (EXIT_FAILURE);
        }

        buffer->buffer = mmap (NULL, buffer->size, PROT_READ | PROT_WRITE, MAP_SHARED, buffer->fd, 0);
        if (buffer->buffer == (void *) -1)
        {
            perror ("mmap failed");
            exit (EXIT_FAILURE);
        }

        memset (buffer->buffer, 0, buffer->size);
        break;
    }
}

/**
 * @brief Release the resources for a memory buffer
 * @param[in,out] buffer The memory buffer to release
 */
void release_memory_buffer (memory_buffer *const buffer)
{
    int rc;

    switch (buffer->allocation_type)
    {
    case BUFFER_ALLOCATION_HEAP:
        free (buffer->buffer);
        break;

    case BUFFER_ALLOCATION_SHARED_MEMORY:
        rc = munmap (buffer->buffer, buffer->size);
        check_assert (rc == 0, "munmap");
        rc = close (buffer->fd);
        check_assert (rc == 0, "close");
        rc = shm_unlink (buffer->pathname);
        if (rc != 0)
        {
            perror ("shm_unlink failed");
            exit (EXIT_FAILURE);
        }
        break;
    }

    buffer->size = 0;
    buffer->buffer = NULL;
    buffer->fd = -1;
}

/**
 * @brief Obtain a pseudo-random 24-bit Packet Sequence Number
 * @return Returns the starting Packet Sequence Number
 */
uint32_t get_random_psn (void)
{
    return lrand48 () & 0xffffff;
}

/**
 * @brief Return the maximum number of inline bytes which can be sent using a Queue Pair
 * @param[in] qp The Queue Pair to get the maximum number of inline bytes for
 * @return Returns the maximum number of inline bytes for qp
 */
uint32_t get_max_inline_data (struct ibv_qp *const qp)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr qp_attr;
    int rc;

    memset (&qp_init_attr, 0, sizeof (qp_init_attr));
    memset (&qp_attr, 0, sizeof (qp_attr));
    rc = ibv_query_qp (qp, &qp_attr, IBV_QP_CAP, &qp_init_attr);
    if (rc != 0)
    {
        perror ("ibv_query_qp failed");
        exit (EXIT_FAILURE);
    }

    return qp_init_attr.cap.max_inline_data;
}

/**
 * @brief Verify that the current state of a Queue Pair is the expected state, aborting on a difference
 * @param[in] expected_state The expected state of the Queue Pair
 * @param[in] qp The Queue Pair to check the state of
 * @param[in] qp_name String which identifies the Queue Pair in an error message
 * @param[in] path_def If non-NULL then used to check if the requested non-default timeout has been set
 */
void verify_qp_state (const enum ibv_qp_state expected_state, struct ibv_qp *const qp, const char *qp_name,
                      const communication_path_definition *const path_def)
{
    int rc;
    struct ibv_qp_attr attr;
    struct ibv_qp_init_attr init_attr;

    rc = ibv_query_qp (qp, &attr, IBV_QP_STATE, &init_attr);
    if (rc != 0)
    {
        perror ("ibv_query_qp failed");
        exit (EXIT_FAILURE);
    }

    if (attr.qp_state != expected_state)
    {
        printf ("%s", qp_name);
        printf (" expected state=%d", expected_state);
        printf (" actual state=%d", attr.qp_state);
        printf ("\n");
        exit (EXIT_FAILURE);
    }

    if ((path_def != NULL) && (path_def->set_non_default_retry_timeout) && (attr.timeout != path_def->retry_timeout))
    {
        printf ("Info: Requested timeout=%u but actual timeout reported by driver is %u\n", path_def->retry_timeout, attr.timeout);
    }
}
