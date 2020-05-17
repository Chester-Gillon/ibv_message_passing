# Build the ibv_time_retry_timeout program

project (ibv_time_retry_timeout C)
cmake_minimum_required (VERSION 2.8)

add_executable (ibv_time_retry_timeout "ibv_time_retry_timeout.c")
target_link_libraries (ibv_time_retry_timeout ibverbs rt)