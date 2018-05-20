# Build the ibv_display_infiniband_counters program

project (ibv_display_infiniband_counters C)
cmake_minimum_required (VERSION 2.8)

# This is required to include ib_types.h
include_directories ("/usr/include/infiniband")

add_executable (ibv_display_infiniband_counters "ibv_display_infiniband_counters_main.c")

target_link_libraries (ibv_display_infiniband_counters ibverbs ibmad)
