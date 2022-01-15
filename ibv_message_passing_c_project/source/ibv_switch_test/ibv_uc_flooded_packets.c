/*
 * @file ibv_uc_flooded_packets.c
 * @date 15 Jan 2022
 * @author Chester Gillon
 * @details Test created to investigate the optimum way to select the host port directions to minimise the number of frames
 *          flooded when using a IBV_QPT_UC to perform a switch test.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>


/* Defines the range of switch ports tested */
#define MIN_PORTS_TESTED 2
#define MAX_PORTS_TESTED 48

/* Two host ports are used, to alternate the source and destination for test frames to allow UC Queue-Pairs to be used
 * which must be on different ports to force frames to be output on the interfaces (rather than being looped back internally). */
#define NUM_HOST_PORTS 2

/* Defines one unique destination MAC, based upon each VLAN on each host port having its own MAC address */
typedef struct
{
    uint32_t vlan;
    uint32_t host_port;
} unique_mac_t;

/* The options for how the host ports directions are changed during the test */
typedef enum
{
    /* The direction of the host ports reverses every frame, except once all port combinations tested */
    DIRECTION_REVERSE_EXCEPT_ALL_PORT_COMBINATIONS_TESTED,
    /* The direction of the host ports reverses every frame, except once the destination_tested_port_index resets */
    DIRECTION_REVERSE_EXCEPT_DESTINATION_TESTED_PORT_INDEX_RESETS,
    /* The direction of the host ports only reverses once all port combinations have been tested */
    DIRECTION_CHANGE_ALL_PORT_COMBINATIONS_TESTED,

    DIRECTION_ARRAY_SIZE
} host_port_direction_option_t;

static const char *const host_port_direction_options[DIRECTION_ARRAY_SIZE] =
{
    [DIRECTION_REVERSE_EXCEPT_ALL_PORT_COMBINATIONS_TESTED] = "reverse except all port combinations tested",
    [DIRECTION_REVERSE_EXCEPT_DESTINATION_TESTED_PORT_INDEX_RESETS] = "reverse exception destination_tested_port_index resets",
    [DIRECTION_CHANGE_ALL_PORT_COMBINATIONS_TESTED] = "change all port combinations tested"
};

#define MAX_DESTINATION_MACS (NUM_HOST_PORTS * MAX_PORTS_TESTED)

/* The summary for one tested combination */
typedef struct
{
    uint32_t num_ports_tested;
    uint32_t num_macs_known;
    uint32_t num_frames_flooded;
    uint32_t num_tx_frames;
    uint32_t iteration_of_last_frame_flooded;
    host_port_direction_option_t host_port_direction_option;
} summary_results_t;

int main (int argc, char *argv[])
{
    summary_results_t summary_results[DIRECTION_ARRAY_SIZE * MAX_PORTS_TESTED] = {0};
    uint32_t num_summary_results = 0;

    /* Iterate over all numbers of port tested, to check for consistent results regardless of the number of ports tested */
    for (uint32_t num_ports_tested = MIN_PORTS_TESTED; num_ports_tested <= MAX_PORTS_TESTED; num_ports_tested++)
    {

        for (host_port_direction_option_t host_port_direction_option = 0;
                host_port_direction_option < DIRECTION_ARRAY_SIZE;
                host_port_direction_option++)
        {
            summary_results_t *const results = &summary_results[num_summary_results++];
            unique_mac_t known_macs[MAX_DESTINATION_MACS] = {0};
            uint32_t host_tx_port = 1;
            uint32_t host_rx_port = 2;
            uint32_t destination_tested_port_index = 0;
            uint32_t source_port_offset = 1;

            results->num_ports_tested = num_ports_tested;
            results->num_tx_frames = 0;
            results->num_macs_known = 0;
            results->num_frames_flooded = 0;
            results->iteration_of_last_frame_flooded = 0;
            results->host_port_direction_option = host_port_direction_option;
            printf ("Testing %" PRIu32 " ports with host port direction %s\n",
                    num_ports_tested, host_port_direction_options[host_port_direction_option]);

            /* Perform multiple iterations to check flooding don't keep occurring */
            uint32_t iteration = 0;
            while (iteration < 4)
            {
                uint32_t mac_index;
                const uint32_t destination_port = destination_tested_port_index + 1;
                const uint32_t source_tested_port_index = (destination_tested_port_index + source_port_offset) % num_ports_tested;
                const uint32_t source_port = source_tested_port_index + 1;
                const uint32_t destination_vlan = 1000 + destination_port;
                const uint32_t source_vlan = 1000 + source_port;
                const unique_mac_t destination_mac =
                {
                    .vlan = destination_vlan,
                    .host_port = host_rx_port
                };
                const unique_mac_t source_mac =
                {
                    .vlan = source_vlan,
                    .host_port = host_tx_port
                };

                /* Determine if the destination MAC is known, meaning has been seen as a source MAC */
                bool destination_mac_known = false;
                for (mac_index = 0; (!destination_mac_known) && (mac_index < results->num_macs_known); mac_index++)
                {
                    if ((destination_mac.vlan == known_macs[mac_index].vlan) &&
                        (destination_mac.host_port == known_macs[mac_index].host_port))
                    {
                        destination_mac_known = true;
                    }
                }

                results->num_tx_frames++;
                if (!destination_mac_known)
                {
                    /* If the destination MAC isn't known, the frame will have to be flooded to all but the source port */
                    printf ("Frame %" PRIu32 " at iteration %" PRIu32 " from host port %" PRIu32 " VLAN %" PRIu32 " to host port %" PRIu32 " VLAN %" PRIu32 " flooded to %" PRIu32 " ports\n",
                            results->num_tx_frames, iteration,
                            source_mac.host_port, source_mac.vlan,
                            destination_mac.host_port, destination_mac.vlan,
                            num_ports_tested - 1);
                    results->num_frames_flooded++;
                    results->iteration_of_last_frame_flooded = iteration;
                }

                /* Save the source MAC as known */
                bool source_mac_known = false;
                for (mac_index = 0; (!source_mac_known) && (mac_index < results->num_macs_known); mac_index++)
                {
                    if ((source_mac.vlan == known_macs[mac_index].vlan) &&
                        (source_mac.host_port == known_macs[mac_index].host_port))
                    {
                        source_mac_known = true;
                    }
                }
                if (!source_mac_known)
                {
                    if (results->num_macs_known == MAX_DESTINATION_MACS)
                    {
                        fprintf (stderr, "Too many known MACs\n");
                        exit (EXIT_FAILURE);
                    }
                    known_macs[results->num_macs_known] = source_mac;
                    results->num_macs_known++;
                }

                /* Advance to the next source/destination port tested, and determine when to swap the host ports */
                bool swap_host_ports = host_port_direction_option != DIRECTION_CHANGE_ALL_PORT_COMBINATIONS_TESTED;
                destination_tested_port_index = (destination_tested_port_index + 1) % num_ports_tested;
                if (destination_tested_port_index == 0)
                {
                    if (host_port_direction_option == DIRECTION_REVERSE_EXCEPT_DESTINATION_TESTED_PORT_INDEX_RESETS)
                    {
                        swap_host_ports = false;
                    }
                    source_port_offset++;
                    if (source_port_offset == num_ports_tested)
                    {
                        iteration++;
                        source_port_offset = 1;
                        if (host_port_direction_option == DIRECTION_REVERSE_EXCEPT_ALL_PORT_COMBINATIONS_TESTED)
                        {
                            swap_host_ports = false;
                        }
                        else if (host_port_direction_option == DIRECTION_CHANGE_ALL_PORT_COMBINATIONS_TESTED)
                        {
                            swap_host_ports = true;
                        }
                    }
                }

                if (swap_host_ports)
                {
                    const uint32_t current_host_rx_port = host_rx_port;

                    host_rx_port = host_tx_port;
                    host_tx_port = current_host_rx_port;
                }
            }

            printf ("Num tx frames %" PRIu32 " num MACs known %" PRIu32 "\n\n", results->num_tx_frames, results->num_macs_known);
        }
    }

    /* Display a summary table */
    printf ("Num    Known  Num      Tx      Last flood\n");
    printf ("ports  MACs   flooded  frames  iteration   Host port direction option\n");
    for (uint32_t summary_index = 0; summary_index < num_summary_results; summary_index++)
    {
        const summary_results_t *const results = &summary_results[summary_index];

        printf ("%5" PRIu32 "  %5" PRIu32 "  %7" PRIu32 "  %6" PRIu32 "  %10" PRIu32 "  %s\n",
                results->num_ports_tested, results->num_macs_known, results->num_frames_flooded, results->num_tx_frames,
                results->iteration_of_last_frame_flooded,
                host_port_direction_options[results->host_port_direction_option]);
    }

    return EXIT_SUCCESS;
}
