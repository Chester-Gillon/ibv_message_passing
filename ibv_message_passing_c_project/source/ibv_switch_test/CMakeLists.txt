# Build the ibv_switch_test programs

project (ibv_switch_test C)
cmake_minimum_required (VERSION 2.8)

add_executable (ibv_raw_packet_tx "ibv_raw_packet_tx.c")
target_link_libraries (ibv_raw_packet_tx ibverbs)

add_executable (ibv_uc_flooded_packets "ibv_uc_flooded_packets.c")