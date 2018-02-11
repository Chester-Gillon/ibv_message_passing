/*
 * @file ibv_message_bw_communication_context.c
 * @date 10 Feb 2018
 * @author Chester Gillon
 * @brief Contains functions which allow multiple communication paths to be used by a program
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"

/** The definitions of all communication paths which have been registered */
#define MAX_PATHS 32
static communication_path_definition paths[MAX_PATHS];
static uint32_t num_paths;

/** Contains the context used to transmit or receive messages on multiple communication paths for one node */
typedef struct communication_context_s
{
    /** Identifies which node this communication context is for */
    int node_number;
    /** The number of transmit paths for this this node is the source */
    uint32_t num_tx_paths;
    /** The array of transmit paths for this this node is the source */
    tx_message_context_handle *tx_paths;
    /** The number of receive paths for which this node is the destination */
    uint32_t num_rx_paths;
    /** The array of receive paths for this node is the destination */
    rx_message_context_handle *rx_paths;
    /** Used to poll the receive paths in round-robin order */
    uint32_t rx_path_index_of_last_message;
} communication_context;

/**
 * @brief Add a path to the list of registered paths
 * @param[in] path_def The path definition to register.
 */
void register_path_definition (const communication_path_definition *const path_def)
{
    CHECK_ASSERT (num_paths < MAX_PATHS);
    paths[num_paths] = *path_def;
    num_paths++;
}

/**
 * @brief Initialise the communication context for a node, which initialises all paths used by node
 * @param[in] node_number Which node to initialise the communication context for
 * @return Returns the allocated context, for which all paths used by the node have been connected
 */
communication_context_handle communication_context_initialise (const int node_number)
{
    communication_context *const context = cache_line_aligned_calloc (1, sizeof (communication_context));
    uint32_t path_index;
    uint32_t tx_path_index;
    uint32_t rx_path_index;

    /* Count the number of transmit and receive paths used by this node */
    context->node_number = node_number;
    context->rx_path_index_of_last_message = 0;
    context->num_tx_paths = 0;
    context->num_rx_paths = 0;
    for (path_index = 0; path_index < num_paths; path_index++)
    {
        const communication_path_definition *const path_def = &paths[path_index];

        if (path_def->source_node == context->node_number)
        {
            context->num_tx_paths++;
        }
        if (path_def->destination_node == context->node_number)
        {
            context->num_rx_paths++;
        }
    }

    /* Create the local resources for the paths used by this node */
    context->tx_paths = cache_line_aligned_calloc (context->num_tx_paths, sizeof (tx_message_context_handle));
    context->rx_paths = cache_line_aligned_calloc (context->num_rx_paths, sizeof (rx_message_context_handle));
    tx_path_index = 0;
    rx_path_index = 0;
    for (path_index = 0; path_index < num_paths; path_index++)
    {
        const communication_path_definition *const path_def = &paths[path_index];

        if (path_def->source_node == context->node_number)
        {
            context->tx_paths[tx_path_index] = message_transmit_create_local (path_def);
            tx_path_index++;
        }
        if (path_def->destination_node == context->node_number)
        {
            context->rx_paths[rx_path_index] = message_receive_create_local (path_def);
            rx_path_index++;
        }
    }

    /* Perform the first stage of attaching to the remote resources for the paths used by this node */
    for (tx_path_index = 0; tx_path_index < context->num_tx_paths; tx_path_index++)
    {
        message_transmit_attach_remote_pre_rtr (context->tx_paths[tx_path_index]);
    }

    for (rx_path_index = 0; rx_path_index < context->num_rx_paths; rx_path_index++)
    {
        message_receive_attach_remote_pre_rtr (context->rx_paths[rx_path_index]);
    }

    /* Perform the second stage of attaching to the remote resources for the paths used by this node */
    for (tx_path_index = 0; tx_path_index < context->num_tx_paths; tx_path_index++)
    {
        message_transmit_attach_remote_post_rtr (context->tx_paths[tx_path_index]);
    }

    for (rx_path_index = 0; rx_path_index < context->num_rx_paths; rx_path_index++)
    {
        message_receive_attach_remote_post_rtr (context->rx_paths[rx_path_index]);
    }

    return context;
}

/**
 * @brief Close the paths for a communication context, freeing the resources
 * @param[in,out] context The communication context for the node to finalise
 */
void communication_context_finalise (communication_context_handle context)
{
    uint32_t path_index;

    for (path_index = 0; path_index < context->num_tx_paths; path_index++)
    {
        await_all_outstanding_messages_freed (context->tx_paths[path_index]);
    }
    for (path_index = 0; path_index < context->num_rx_paths; path_index++)
    {
        message_receive_finalise (context->rx_paths[path_index]);
    }
    for (path_index = 0; path_index < context->num_tx_paths; path_index++)
    {
        message_transmit_finalise (context->tx_paths[path_index]);
    }
    free (context);
}

/**
 * @brief Return the handle for a transmit path in the communication context for one node
 * @param[in] context The communication context for the node
 * @param[in] destination_node The destination node to find the transmit handle for
 * @param[in] instance The ID of the communication path to find the transmit handle for
 */
tx_message_context_handle get_tx_path_handle (communication_context_handle context,
                                              const int destination_node, const int instance)
{
    tx_message_context_handle tx_handle = NULL;
    uint32_t path_index;
    uint32_t tx_path_index;

    tx_path_index = 0;
    for (path_index = 0; (tx_handle == NULL) && (path_index < num_paths); path_index++)
    {
        const communication_path_definition *const path_def = &paths[path_index];

        if (path_def->source_node == context->node_number)
        {
            if ((path_def->destination_node == destination_node) && (path_def->instance == instance))
            {
                tx_handle = context->tx_paths[tx_path_index];
            }
            tx_path_index++;
        }
    }
    CHECK_ASSERT (tx_handle != NULL);

    return tx_handle;
}

/**
 * @brief Poll for receipt of a message from any of the receive paths in a communication context
 * @param[in] context The communication context to poll for receipt of a message for
 * @return Returns the received message, or NULL if no message available
 */
rx_api_message_buffer *poll_rx_paths (communication_context_handle context)
{
    rx_api_message_buffer *buffer = NULL;
    uint32_t rx_path_index;
    uint32_t num_paths_polled;

    num_paths_polled = 0;
    rx_path_index = (context->rx_path_index_of_last_message + 1) % context->num_rx_paths;
    while ((buffer == NULL) && (num_paths_polled < context->num_rx_paths))
    {
        buffer = poll_rx_message (context->rx_paths[rx_path_index]);
        if (buffer != NULL)
        {
            context->rx_path_index_of_last_message = rx_path_index;
        }
        else
        {
            rx_path_index = (rx_path_index + 1) % context->num_rx_paths;
        }
    }

    return buffer;
}

/**
 * @brief Wait for receipt of a message from any of the receive paths in a communication context
 * @param[in] context The communication context to wait for a receive message from
 * @return Returns the received message
 */
rx_api_message_buffer *await_any_rx_message (communication_context_handle context)
{
    rx_api_message_buffer *buffer;

    do
    {
        buffer = poll_rx_paths (context);
    } while (buffer == NULL);

    return buffer;
}
