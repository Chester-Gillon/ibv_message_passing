# Build the programs which measure Infiniband message latency between PCs

project (ibv_message_latency C)
cmake_minimum_required (VERSION 2.9)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_library (ibv_message_latency_messages "ibv_message_latency_messages.c")

add_executable (ibv_message_latency_master "ibv_message_latency_master.c")
target_link_libraries (ibv_message_latency_master ibv_message_latency_messages
                       ibv_message_transport ibverbs rt slp)

add_executable (ibv_message_latency_slave "ibv_message_latency_slave.c")
target_link_libraries (ibv_message_latency_slave ibv_message_latency_messages
                       ibv_message_transport ibverbs rt slp)
