# Build the ibv_round_trip_delay program

project (ibv_message_bw C)
cmake_minimum_required (VERSION 2.9)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_round_trip_delay "ibv_round_trip_delay_main.c")
target_link_libraries (ibv_round_trip_delay ibv_message_transport ibverbs sctp rt slp pthread numa)
