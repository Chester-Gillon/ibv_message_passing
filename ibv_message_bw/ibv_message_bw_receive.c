/*
 * @file ibv_message_bw_receive.c
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains functions to receive messages on a communication path for the ibv_message_bw program
 */

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"

/** Contains the context used to receive messages from one communication path over a pair of connected Infiniband ports */
typedef struct rx_message_context_s
{
    /** The definition of the communication path for which this context receives messages on */
    communication_path_definition path_def;
    /** The Infiniband port this context receives messages on */
    ib_port_endpoint endpoint;
    /** Used to exchange Queue Pair information with the transmit end of the communication path */
    communication_path_slp_connection slp_connection;
} rx_message_context;

/**
 * @details Perform the initialisation of the context for the reception of messages on a communication path
 *          which creates local resources without requiring the identity of the Queue Pair on the source.
 *
 *          This publishes the Queue Pair of the receive endpoint using SLP, so that the transmit end point can connect.
 * @param[in] path_def Defines the communication path to create the receive end for
 * @return Returns a pointer to the context to receive messages on
 */
rx_message_context_handle message_receive_create_local (const communication_path_definition *const path_def)
{
    rx_message_context_handle context = cache_line_aligned_calloc (1, sizeof (rx_message_context));

    context->path_def = *path_def;
    open_ib_port_endpoint (&context->endpoint, &context->path_def);
    intialise_slp_connection (&context->slp_connection, false, &context->path_def);

    return context;
}

/**
 * @details Complete the initialisation of the context for the reception of messages on a communication path by:
 *          - Transition the receive Queue Pair, used to send freed sequence numbers, to the Ready To Send State
 *          - Initialise the free sequence number transmit buffers
 *
 *          This obtains the Queue Pair and memory region for the transmit context from SLP
 * @param[in,out] context The receive message context to complete the initialisation for
 */
void message_receive_attach_remote (rx_message_context_handle context)
{

}

/**
 * @brief Free the resources used by the context for the reception of messages on a communication path
 * @param[in,out] context The receive message context to free the resources for
 */
void message_receive_finalise (rx_message_context_handle context)
{
    close_slp_connection (&context->slp_connection);
    close_ib_port_endpoint (&context->endpoint);
    free (context);
}
