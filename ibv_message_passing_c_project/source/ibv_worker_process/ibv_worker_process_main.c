/*
 * @file ibv_worker_process_main.c
 * @date 11 Feb 2018
 * @author Chester Gillon
 * @details The main for a worker process written in C which communicates with a controller process written in Ada
 *          via the ibv_message_transport library.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"
#include "ibv_controller_worker_messages.h"

/** Identifies which worker instance this process is */
static int worker_node_number;

/** Command line option for configuring the communication paths */
static bool controller_and_workers_on_separate_pcs;

/** The absolute pathname of the executable for this process */
static char worker_executable_pathname[PATH_MAX];


/**
 * @brief Parse the command line arguments for the worker process
 * @param[in] argc Argument count
 * @param[in] argv Argument values
 */
static void parse_command_line_arguments (int argc, char *argv[])
{
    const char *program_name = argv[0];
    const char *worker_node_number_string;
    char junk;

    if ((argc < 2) || (argc > 3))
    {
        fprintf (stderr, "Usage: %s <worker_node_number> [<controller_and_workers_on_separate_pcs>]\n", program_name);
        exit (EXIT_FAILURE);
    }

    worker_node_number_string = argv[1];
    if ((sscanf (worker_node_number_string, "%d%c", &worker_node_number, &junk) != 1) ||
        (worker_node_number < FIRST_WORKER_NODE_ID) || (worker_node_number > LAST_WORKER_NODE_ID))
    {
        fprintf (stderr, "%s is not a valid worker_node_number\n", worker_node_number_string);
        exit (EXIT_FAILURE);
    }

    if (argc > 2)
    {
        controller_and_workers_on_separate_pcs = atoi (argv[2]);
    }

    if (realpath (program_name, worker_executable_pathname) == NULL)
    {
        fprintf (stderr, "Failed to resolve pathname for executable\n");
        exit (EXIT_FAILURE);
    }
}

/**
 * @brief Send a message to the controller to indicate the worker is ready to run the test
 * @param[in] path_to_controller The communication path to the controller
 */
static void report_worker_ready (tx_message_context_handle path_to_controller)
{
    tx_api_message_buffer *const tx_buffer = get_send_buffer (path_to_controller);
    worker_ready_msg *const worker_ready = tx_buffer->data;

    tx_buffer->header->message_id = CW_WORKER_READY;
    tx_buffer->header->source_instance = worker_node_number;
    tx_buffer->header->message_length = sizeof (worker_ready_msg);
    worker_ready->worker_executable_pathname_len = strlen (worker_executable_pathname);
    memcpy (worker_ready->worker_executable_pathname, worker_executable_pathname,
            sizeof (worker_ready->worker_executable_pathname));
    send_message (tx_buffer);
}

/**
 * @brief Process a CW_SUM_INTEGERS message from the controller, returning a CW_SUM_RESULT message in response
 * @param[in] sum_integers The message from the controller to process
 * @param[in] path_to_controller The communication path to the controller
 */
static void process_sum_integers_request (const sum_integers_msg *const sum_integers, tx_message_context_handle path_to_controller)
{
    tx_api_message_buffer *const tx_buffer = get_send_buffer (path_to_controller);
    sum_result_msg *const sum_result = tx_buffer->data;
    uint32_t data_index;

    tx_buffer->header->message_id = CW_SUM_RESULT;
    tx_buffer->header->source_instance = worker_node_number;
    tx_buffer->header->message_length = sizeof (sum_result_msg);
    sum_result->request_id = sum_integers->request_id;
    sum_result->sum = 0;
    for (data_index = 0; data_index < sum_integers->num_integers_to_sum; data_index++)
    {
        sum_result->sum += sum_integers->integers_to_sum[data_index];
    }
    send_message (tx_buffer);
}

/**
 * @brief Perform the worker test process by executing commands from the controller, exiting when requested to shutdown.
 * @param[in] communication_context Used to receive messages from the controller
 * @param[in] path_to_controller Used to send messages to the controller
 */
static void perform_worker_test (communication_context_handle communication_context,
                                 tx_message_context_handle path_to_controller)
{
    bool test_complete = false;
    rx_api_message_buffer *rx_buffer;
    uint32_t worker_index;

    while (!test_complete)
    {
        rx_buffer = await_any_rx_message (communication_context);

        switch (rx_buffer->header->message_id)
        {
        case CW_REQUEST_SHUTDOWN:
            {
                const request_shutdown_msg *const shutdown_request = rx_buffer->data;

                for (worker_index = 0; worker_index < NUM_WORKERS; worker_index++)
                {
                    printf ("Worker %u processed %u requests\n", worker_index + FIRST_WORKER_NODE_ID,
                            shutdown_request->num_requests_per_worker[worker_index]);
                }
                test_complete = true;
            }
            break;

        case CW_SUM_INTEGERS:
            {
                const sum_integers_msg *const sum_integers = rx_buffer->data;

                process_sum_integers_request (sum_integers, path_to_controller);
            }
            break;

        default:
            check_assert (false, "perform_worker_test Unknown message_id");
            break;
        }

        free_message (rx_buffer);
    }
}

int main (int argc, char *argv[])
{
    communication_context_handle communication_context;
    tx_message_context_handle path_to_controller;

    parse_command_line_arguments (argc, argv);
    register_controller_worker_messages (controller_and_workers_on_separate_pcs);
    communication_context = communication_context_initialise (worker_node_number);
    path_to_controller = get_tx_path_handle (communication_context, CONTROLLER_NODE_ID, WORKER_TO_CONTROLLER_PATH);
    report_worker_ready (path_to_controller);
    perform_worker_test (communication_context, path_to_controller);
    communication_context_finalise (communication_context);

    return EXIT_SUCCESS;
}
