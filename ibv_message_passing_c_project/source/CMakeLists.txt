# Top level CMake file

project (ibv_message_passing C CXX)
cmake_minimum_required (VERSION 2.8)

include (CheckStructHasMember)

# Set platform build flags
if ("${PLATFORM_TYPE}" STREQUAL "debug")
   set (PLATFORM_CFLAGS "-g")
elseif ("${PLATFORM_TYPE}" STREQUAL "release")
   set (PLATFORM_CFLAGS "-O3 -g -flto -fuse-linker-plugin")
elseif ("${PLATFORM_TYPE}" STREQUAL "coverage")
   set (PLATFORM_CFLAGS "-g --coverage -fprofile-update=atomic --save-temps")
elseif ("${PLATFORM_TYPE}" STREQUAL "aarch64_debug")
   set (PLATFORM_CFLAGS "-Werror=poison-system-directories -g")
elseif ("${PLATFORM_TYPE}" STREQUAL "aarch64_release")
   set (PLATFORM_CFLAGS "-Werror=poison-system-directories -O3 -g -flto -fuse-linker-plugin")
else()
   message ("Unknown platform")
endif()

if (AARCH64_SYSROOT)
   # Add cross-compiled include and library paths for packages used directly by the source code
   include_directories("${AARCH64_SYSROOT}/rdma-core/usr/include")
   link_directories("${AARCH64_SYSROOT}/rdma-core/usr/lib")
   include_directories("${AARCH64_SYSROOT}/openslp/usr/include")
   link_directories("${AARCH64_SYSROOT}/openslp/usr/lib")
   include_directories("${AARCH64_SYSROOT}/numactl/usr/include")
   link_directories("${AARCH64_SYSROOT}/numactl/usr/lib")
   include_directories("${AARCH64_SYSROOT}/libftdi/usr/include/libftdi1")
   link_directories("${AARCH64_SYSROOT}/libftdi/usr/lib")
   include_directories("${AARCH64_SYSROOT}/lksctp-tools/usr/include")
   link_directories("${AARCH64_SYSROOT}/lksctp-tools/usr/lib")
   include_directories("${AARCH64_SYSROOT}/net-snmp/usr/include")
   link_directories("${AARCH64_SYSROOT}/net-snmp/usr/lib")
   include_directories("${AARCH64_SYSROOT}/openssl/usr/include")
   link_directories("${AARCH64_SYSROOT}/openssl/usr/lib")

   # Add cross-compiled library paths for packages referenced by libraries above
   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath-link=${AARCH64_SYSROOT}/libnl/usr/lib")
   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath-link=${AARCH64_SYSROOT}/libpcre/usr/lib")
   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath-link=${AARCH64_SYSROOT}/libusb1/lib")
   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath-link=${AARCH64_SYSROOT}/eudev/lib")
   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath-link=${AARCH64_SYSROOT}/openssl/usr/lib")
endif()

# Set conditional compilation flags 
# @todo cap_user_rdpmc is a bit-field and CheckStructHasMember fails to find bit-fields since the C source
#       used to check the field tries to take the sizeof the field which is illegal for a bit field.
#       Therefore test for the capabilities field instead which is an integer.
CHECK_STRUCT_HAS_MEMBER ("struct perf_event_mmap_page" capabilities linux/perf_event.h HAVE_PERF_CAP_USER_RDPMC)
CHECK_STRUCT_HAS_MEMBER ("struct perf_event_mmap_page" pmc_width linux/perf_event.h HAVE_PERF_PMC_WIDTH)
if (${HAVE_PERF_CAP_USER_RDPMC})
    add_definitions(-DHAVE_PERF_CAP_USER_RDPMC)
endif()
if (${HAVE_PERF_PMC_WIDTH})
    add_definitions(-DHAVE_PERF_PMC_WIDTH)
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
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_display_local_infiniband_port_statistics")
add_subdirectory ("${PROJECT_SOURCE_DIR}/display_kernel_threads")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_c_structure_size")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_worker_process")
add_subdirectory ("${PROJECT_SOURCE_DIR}/test_multithreaded_mempolicy")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_functional_loopback_test")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_pingpong_measurements")
add_subdirectory ("${PROJECT_SOURCE_DIR}/virtual_inheritence")
add_subdirectory ("${PROJECT_SOURCE_DIR}/gdb_unblock_sigchld")
add_subdirectory ("${PROJECT_SOURCE_DIR}/vector_test_load")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_time_sync_measurement")
add_subdirectory ("${PROJECT_SOURCE_DIR}/floor_it")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_message_latency")
add_subdirectory ("${PROJECT_SOURCE_DIR}/log10f_test")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_time_dma_flush")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_monitor_port_state")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_time_retry_timeout")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_generate_infiniband_test_load")
add_subdirectory ("${PROJECT_SOURCE_DIR}/gdb_debug_large_structure")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_spawn_workers_and_controller")
add_subdirectory ("${PROJECT_SOURCE_DIR}/parse_debug_information")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_time_rnr")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_round_trip_delay")
add_subdirectory ("${PROJECT_SOURCE_DIR}/usb_id_dongle")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_display_local_infiniband_device_statistics")
add_subdirectory ("${PROJECT_SOURCE_DIR}/snmp_response_time")
add_subdirectory ("${PROJECT_SOURCE_DIR}/ibv_write_index_rdma_ordering")
add_subdirectory ("${PROJECT_SOURCE_DIR}/serial_handshake_dongle")

# Location of the source code for the linuxptp package, to gain access to the phc.[ch]
set (LINUXPTP_ROOT "$ENV{HOME}/linuxptp-1.6")

if (EXISTS ${LINUXPTP_ROOT})
    # Generate a definition to re-direct the include of phc.h via a macro, to test the CMake IMPLICIT_DEPENDS_INCLUDE_TRANSFORM.
    # Where IMPLICIT_DEPENDS_INCLUDE_TRANSFORM is set for this root directory and subdirectories.
    # IMPLICIT_DEPENDS_INCLUDE_TRANSFORM has to include the complete filename in the replacement filename, rather than using
    # the macro argument since CMake doesn't pre-process the macro argument.
    #
    # For the Eclipse indexer to resolve the PTP_INCLUDE macro in read_ptp.c the PTP_MAPPING symbol needs to set in the Eclipse
    # "Paths and Symbols" in the .cproject file.
    #
    # CMake has followed the dependency correcly when the generated read_ptp/CMakeFiles/read_ptp.dir/build.make contains the
    # following:
    #   read_ptp/CMakeFiles/read_ptp.dir/read_ptp.c.o: /home/mr_halfword/linuxptp-1.6/missing.h
    #   read_ptp/CMakeFiles/read_ptp.dir/read_ptp.c.o: /home/mr_halfword/linuxptp-1.6/phc.h
    set (PTP_MAPPING phc)
    add_definitions(-DPTP_MAPPING=${PTP_MAPPING})
    set_property (DIRECTORY PROPERTY IMPLICIT_DEPENDS_INCLUDE_TRANSFORM "PTP_INCLUDE(%)=<${PTP_MAPPING}.h>")

    add_subdirectory ("${PROJECT_SOURCE_DIR}/read_ptp")
endif()

# Add a custom target which builds the GPR project which contains all the Ada exececutables.
# This target depends on the Ada interface specs and library generated by the ibv_message_transport C project,
# so the CMake generated makefiles don't start to compile the Ada executables until the dependencies are available.
add_custom_target (ibv_message_passing_ada_project ALL
                   gnatmake -r -P${CMAKE_SOURCE_DIR}/../../ibv_message_passing_ada_project/ibv_message_passing_ada_project_link.gpr -p -Xmode=${PLATFORM_TYPE}
                   DEPENDS ibv_message_transport ibv_message_transport_ada_spec)
