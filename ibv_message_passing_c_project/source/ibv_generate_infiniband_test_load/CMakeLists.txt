# Build the ibv_generate_infiniband_test_load program

project (ibv_generate_infiniband_test_load C)
cmake_minimum_required (VERSION 2.8)

# This is required to include ib_types.h
include_directories ("/usr/include/infiniband")

add_executable (ibv_generate_infiniband_test_load "ibv_generate_infiniband_test_load.c")
target_link_libraries (ibv_generate_infiniband_test_load numa ibverbs ibmad rt)