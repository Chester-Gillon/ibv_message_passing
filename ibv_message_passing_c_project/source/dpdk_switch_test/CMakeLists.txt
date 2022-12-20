# Build the dpdk_switch_test programs

project (dpdk_switch_test C)

include_directories (${DPDK_INCLUDE_DIRS})

add_executable (dpdk_information "dpdk_information.c")
target_link_libraries (dpdk_information ${DPDK_LIBRARIES})

add_executable (dpdk_switch_test "dpdk_switch_test.c")
target_link_libraries (dpdk_switch_test ${DPDK_LIBRARIES} pthread m)