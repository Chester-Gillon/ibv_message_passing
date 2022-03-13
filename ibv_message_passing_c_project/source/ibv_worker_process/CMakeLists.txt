# Build the ibv_worker_process program

project (ibv_worker_process C)
cmake_minimum_required (VERSION 2.9)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_worker_process "ibv_worker_process_main.c")

target_link_libraries (ibv_worker_process ibv_message_transport ibverbs rt slp)
