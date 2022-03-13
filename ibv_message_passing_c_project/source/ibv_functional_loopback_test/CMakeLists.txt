# Build the ibv_functional_loopback_test program

project (ibv_functional_loopback_test C)
cmake_minimum_required (VERSION 2.9)

# This is required to include ib_types.h
include_directories ("/usr/include/infiniband")

add_executable (ibv_functional_loopback_test "ibv_functional_loopback_test_main.c"
                                             "ibv_utils.c"
                                             "sender_rdma_write_receiver_passive.c"
                                             "sender_rdma_write_with_imm_receiver_poll_cq.c"
                                             "sender_send_receiver_recv.c")

target_link_libraries (ibv_functional_loopback_test pthread ibverbs ibmad rt)
