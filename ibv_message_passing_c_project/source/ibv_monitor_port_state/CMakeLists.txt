# Build the ibv_monitor_port_state program

project (ibv_monitor_port_state C)
cmake_minimum_required (VERSION 2.8)

add_executable (ibv_monitor_port_state "ibv_monitor_port_state.c")
target_link_libraries (ibv_monitor_port_state ibverbs rt)
