# Build the ibv_spawn_workers_and_controller program

project (ibv_spawn_workers_and_controller C)
cmake_minimum_required (VERSION 2.8)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_spawn_workers_and_controller "ibv_spawn_workers_and_controller.c")
target_link_libraries (ibv_spawn_workers_and_controller ibv_shared_library_test ibv_message_transport ibverbs rt slp pthread)

# Disable default of setting the RPATH to the build path as testing shared library path handling.
set_target_properties (ibv_spawn_workers_and_controller PROPERTIES SKIP_BUILD_RPATH true)

add_executable (ibv_shared_library_user "ibv_shared_library_user.c")
target_link_libraries (ibv_shared_library_user ibv_shared_library_test)
set_target_properties (ibv_shared_library_user PROPERTIES SKIP_BUILD_RPATH true)

add_library(ibv_shared_library_test SHARED "ibv_shared_library_test.c")
