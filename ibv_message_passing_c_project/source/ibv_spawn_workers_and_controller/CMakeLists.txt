# Build the ibv_spawn_workers_and_controller program

project (ibv_spawn_workers_and_controller C)
cmake_minimum_required (VERSION 2.8)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_spawn_workers_and_controller "ibv_spawn_workers_and_controller.c")
target_link_libraries (ibv_spawn_workers_and_controller ibv_message_transport ibverbs rt slp pthread)
