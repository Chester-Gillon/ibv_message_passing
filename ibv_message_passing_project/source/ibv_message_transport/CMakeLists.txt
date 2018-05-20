# Build the ibv_message_transport library

project (ibv_message_transport C)
cmake_minimum_required (VERSION 2.8)

add_library (ibv_message_transport "ibv_controller_worker_messages.c"
                                   "ibv_message_bw_common.c"
                                   "ibv_message_bw_communication_context.c"
                                   "ibv_message_bw_receive.c"
                                   "ibv_message_bw_transmit.c")
