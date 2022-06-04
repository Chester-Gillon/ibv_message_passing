# Build the ibv_monitor_port_state program

project (ibv_monitor_port_state C)

add_executable (ibv_monitor_port_state "ibv_monitor_port_state.c")
target_link_libraries (ibv_monitor_port_state ibverbs rt)
