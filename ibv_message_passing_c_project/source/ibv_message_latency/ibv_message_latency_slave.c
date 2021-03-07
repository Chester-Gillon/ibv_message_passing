/*
 * @file ibv_message_latency_slave.c
 * @date 18 Nov 2018
 * @author Chester Gillon
 * @brief The slave for the latency measurement tests
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include <time.h>
#include <sys/mman.h>

#include "ibv_message_bw_interface.h"
#include "ibv_message_latency_messages.h"


int main (int argc, char *argv[])
{
    int rc;
    communication_context_handle communication_context;
    tx_message_context_handle path_to_master;
    uint32_t service_levels[MESSAGE_LATENCY_MAX_PARALLEL_PATHS];
    rx_api_message_buffer *rx_buffers[MESSAGE_LATENCY_MAX_PARALLEL_PATHS] = {NULL};
    tx_api_message_buffer *tx_buffer;
    uint32_t rx_index;

    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s <comma_delimited_service_levels>\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    if (rc != 0)
    {
        perror ("mlockall");
        exit (EXIT_FAILURE);
    }

    const uint32_t num_data_paths = register_message_latency_messages (argv[1], service_levels);
    communication_context = communication_context_initialise (MESSAGE_LATENCY_SLAVE_NODE_ID);
    path_to_master = get_tx_path_handle (communication_context, MESSAGE_LATENCY_MASTER_NODE_ID, SLAVE_ACK_PATH);
    bool test_complete = false;

    /* Run test until told by the master to exit */
    do
    {
        /* Get message buffer to send the ack with the data message timestamps */
        tx_buffer = get_send_buffer (path_to_master);
        latency_ack_msg *const latency_ack = tx_buffer->data;
        tx_buffer->header->message_id = MESSAGE_LATENCY_ACK_MSG_ID;
        tx_buffer->header->message_length = sizeof (*latency_ack);

        /* Receive all data messages for one iteration, saving the receive time.
         * The messages may not be received in path order, so use the source_instance to store the receive time of the path */
        for (rx_index = 0; rx_index < num_data_paths; rx_index++)
        {
            rx_buffers[rx_index] = await_any_rx_message (communication_context);
            rc = clock_gettime (CLOCK_REALTIME, &latency_ack->message_rx_times[rx_buffers[rx_index]->header->source_instance]);
            CHECK_ASSERT (rc == 0);
        }

        /* Send an ack to the master to containing the message receive times, unless the test is complete */
        for (rx_index = 0; rx_index < num_data_paths; rx_index++)
        {
            if (rx_buffers[rx_index]->header->message_id == MESSAGE_LATENCY_TEST_COMPLETE_MSG_ID)
            {
                test_complete = true;
            }
            free_message (rx_buffers[rx_index]);
            rx_buffers[rx_index] = NULL;
        }
        send_message (tx_buffer);
    } while (!test_complete);

    communication_context_finalise (communication_context);

    return EXIT_SUCCESS;
}
