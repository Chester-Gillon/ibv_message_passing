/*
 * @file ibv_time_sync_measurement_slave.c
 * @date 16 Sep 2018
 * @author Chester Gillon
 * @brief The slave program for time synchronisation measurement
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>
#include <sys/mman.h>

#include "ibv_message_bw_interface.h"
#include "ibv_time_sync_measurement_messages.h"


/**
 * @brief Run the time synchronisation slave until told by the master to exit.
 * @details The slave doesn't maintain any statistics on the time synchronisation, since all statistics are maintained by the
 *          master.
 * @param[in] argc, argv
 *            Command line arguments are not used, as the Infiniband ports used are hard coded.
 */
int main (int argc, char *argv[])
{
    communication_context_handle communication_context;
    tx_message_context_handle path_to_master;
    tx_api_message_buffer *tx_buffer;
    rx_api_message_buffer *rx_buffer;
    bool test_complete;
    slave_current_time_msg *slave_time_msg;
    int rc;

    rc = mlockall (MCL_CURRENT | MCL_FUTURE);
    if (rc != 0)
    {
        perror ("mlockall");
        exit (EXIT_FAILURE);
    }

    /* Initialise communication paths */
    register_time_sync_measurement_messages ();
    communication_context = communication_context_initialise (TIME_SYNC_SLAVE_NODE_ID);
    path_to_master = get_tx_path_handle (communication_context, TIME_SYNC_MASTER_NODE_ID, TIME_SYNC_SLAVE_TO_MASTER_PATH);

    /* Wait for the master to be ready */
    rx_buffer = await_any_rx_message (communication_context);
    CHECK_ASSERT (rx_buffer->header->message_id == TIME_SYNC_MASTER_READY_MSG_ID);
    free_message (rx_buffer);

    /* Tell the master we are ready */
    tx_buffer = get_send_buffer (path_to_master);
    tx_buffer->header->message_id = TIME_SYNC_SLAVE_READY_MSG_ID;
    tx_buffer->header->message_length = 0;
    tx_buffer->header->source_instance = 0;
    send_message (tx_buffer);

    /* Run the test until told by the master to exit */
    test_complete = false;
    while (!test_complete)
    {
        rx_buffer = await_any_rx_message (communication_context);
        switch (rx_buffer->header->message_id)
        {
        case TIME_SYNC_TEST_COMPLETE:
            test_complete = true;
            break;

        case TIME_SYNC_TIME_REQUEST:
            tx_buffer = get_send_buffer (path_to_master);
            tx_buffer->header->message_id = TIME_SYNC_TIME_REPLY;
            tx_buffer->header->message_length = sizeof (slave_current_time_msg);
            tx_buffer->header->source_instance = 0;
            slave_time_msg = tx_buffer->data;
            rc = clock_gettime (CLOCK_REALTIME, &slave_time_msg->slave_current_time);
            CHECK_ASSERT (rc == 0);
            send_message (tx_buffer);
            break;

        default:
            check_assert (false, "ibv_time_sync_measurement_slave Unknown message_id");
            break;
        }
        free_message (rx_buffer);
    }

    communication_context_finalise (communication_context);

    return EXIT_SUCCESS;
}
