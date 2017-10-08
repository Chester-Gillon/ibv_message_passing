/*
 * @file ibv_message_bw_transmit.c
 * @date 8 Oct 2017
 * @author Chester Gillon
 * @brief Contains functions to transmit messages on a communication path for the ibv_message_bw program
 */

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"

/** Contains the context used to transmit messages on one communication path over a pair of connected Infiniband ports */
typedef struct tx_message_context_s
{
    /** The definition of the communication path for which this context transmits messages on */
    communication_path_definition path_def;
    /** The Infiniband port this context transmits messages on */
    ib_port_endpoint endpoint;
    /** Used to exchange Queue Pair information with the receive end of the communication path */
    communication_path_slp_connection slp_connection;
} tx_message_context;

/**
 * @details Perform the initialisation of the context for the transmission of messages on a communication path
 *          which creates local resources without requiring the identity of the Queue Pair on the destination.
 *
 *          This publishes the Queue Pair of the transmit endpoint using SLP, so that the receive endpoint can connect.
 * @param[in] path_def Defines the communication path to create the transmit end for
 * @return Returns a pointer to the context to transmit messages on
 */
tx_message_context_handle message_transmit_create_local (const communication_path_definition *const path_def)
{
    tx_message_context_handle context = cache_line_aligned_calloc (1, sizeof (tx_message_context));

    context->path_def = *path_def;
    open_ib_port_endpoint (&context->endpoint, &context->path_def);
    intialise_slp_connection (&context->slp_connection, true, &context->path_def);

    return context;
}

/**
 * @details Complete the initialisation of the context for the transmission of messages on a communication path by:
 *          - Transition the transmit Queue Pair, used to send messages, to the Ready To Send State
 *          - Initialise the message transmit buffers
 *
 *          This obtains the Queue Pair and memory region for the receive context from SLP
 * @param[in,out] context The transmit message context to complete the initialisation for
 */
void message_transmit_attach_remote (tx_message_context_handle context)
{

}

/**
 * @brief Free the resources used by the context for the transmission of messages on a communication path
 * @param[in,out] context The transmit message context to free the resources for
 */
void message_transmit_finalise (tx_message_context_handle context)
{
    close_slp_connection (&context->slp_connection);
    close_ib_port_endpoint (&context->endpoint);
    free (context);
}
