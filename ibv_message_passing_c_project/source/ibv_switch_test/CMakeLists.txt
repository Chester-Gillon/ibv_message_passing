# Build the ibv_switch_test programs

project (ibv_switch_test C)

if (${HAVE_IBV_WR_START})
    add_executable (ibv_raw_packet_tx "ibv_raw_packet_tx.c")
    target_link_libraries (ibv_raw_packet_tx ibverbs cap)
endif()

add_executable (ibv_uc_flooded_packets "ibv_uc_flooded_packets.c")

add_executable (ibv_uc_switch_test "ibv_uc_switch_test.c")
target_link_libraries (ibv_uc_switch_test pthread ibverbs)