# Build the ibv_message_bw program

project (ibv_message_bw C)
cmake_minimum_required (VERSION 2.8)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_message_bw "ibv_message_bw_main.c")

target_link_libraries (ibv_message_bw ibv_message_transport ibverbs pthread rt slp)
