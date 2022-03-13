# Build the ibv_display_local_infiniband_port_statistics program

project (ibv_display_local_infiniband_port_statistics C)
cmake_minimum_required (VERSION 2.9)

# This is required to include ib_types.h
include_directories ("/usr/include/infiniband")

add_executable (ibv_display_local_infiniband_port_statistics "ibv_display_local_infiniband_port_statistics.c")

target_link_libraries (ibv_display_local_infiniband_port_statistics ibverbs ibmad ibnetdisc rt)
