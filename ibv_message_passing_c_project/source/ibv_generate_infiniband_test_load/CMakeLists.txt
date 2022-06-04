# Build the ibv_generate_infiniband_test_load program

project (ibv_generate_infiniband_test_load C)

add_executable (ibv_generate_infiniband_test_load "ibv_generate_infiniband_test_load.c")
target_link_libraries (ibv_generate_infiniband_test_load numa ibverbs rt)