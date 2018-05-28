# Top level CMake file

project (ibv_message_passing C CXX)
cmake_minimum_required (VERSION 2.8)

# Set platform build flags
if ("${PLATFORM_TYPE}" STREQUAL "Debug")
   set (PLATFORM_CFLAGS "-g")
elseif ("${PLATFORM_TYPE}" STREQUAL "Release")
   set (PLATFORM_CFLAGS "-O3 -g -flto -fuse-linker-plugin")
else()
   message ("Unknown platform")
endif()

# -fmessage-length=0 is to allow Eclipse traceback for error messages
set (COMMON_FLAGS "${PLATFORM_CFLAGS} -Wall -fmessage-length=0")

set (CMAKE_C_FLAGS "${COMMON_FLAGS}")
set (CMAKE_CXX_FLAGS "${COMMON_FLAGS}")

# To use -flto need to use the gcc versions of ar and ranlib
SET(CMAKE_AR "gcc-ar")
SET(CMAKE_RANLIB "gcc-ranlib")

add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_message_transport")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_message_bw")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_display_infiniband_counters")
add_subdirectory ("${PROJECT_SOURCE_DIR}/display_kernel_threads")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_c_structure_size")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_worker_process")
add_subdirectory ("${PROJECT_SOURCE_DIR}/test_multithreaded_mempolicy")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_functional_loopback_test")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_pingpong_measurements")