# Build the ibv_spawn_workers_and_controller program

project (ibv_spawn_workers_and_controller C)
cmake_minimum_required (VERSION 2.8)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_spawn_workers_and_controller "ibv_spawn_workers_and_controller.c")
target_link_libraries (ibv_spawn_workers_and_controller ibv_shared_library_test ibv_message_transport ibverbs rt slp pthread)

# Set the RPATH to be the same directory as the executable, to allow the test shared library to be located when
# the executable is given a capability.
# Did try and use $ORIGIN instead of ./ but that was ignored in the same way as trying to set LD_LIBRARY_PATH
set_target_properties (ibv_spawn_workers_and_controller PROPERTIES BUILD_WITH_INSTALL_RPATH true)
set_target_properties (ibv_spawn_workers_and_controller PROPERTIES INSTALL_RPATH "./")

add_executable (ibv_shared_library_user "ibv_shared_library_user.c")
target_link_libraries (ibv_shared_library_user ibv_shared_library_test)

# The program which is spawned by ibv_spawn_workers_and_controller with a capability also needs to have the same RPATH
# as the spawning process.
set_target_properties (ibv_shared_library_user PROPERTIES BUILD_WITH_INSTALL_RPATH true)
set_target_properties (ibv_shared_library_user PROPERTIES INSTALL_RPATH "./")

add_library(ibv_shared_library_test SHARED "ibv_shared_library_test.c")
