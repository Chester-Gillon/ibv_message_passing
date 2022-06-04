# Build the ibv_display_local_infiniband_port_statistics program

project (ibv_display_local_infiniband_port_statistics C)

# This is required to include ib_types.h
include_directories ("/usr/include/infiniband")

add_executable (ibv_display_local_infiniband_port_statistics "ibv_display_local_infiniband_port_statistics.c")

target_link_libraries (ibv_display_local_infiniband_port_statistics ibverbs ibmad ibnetdisc rt)
