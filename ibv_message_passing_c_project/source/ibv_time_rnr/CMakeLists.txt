# Build the ibv_time_rnr program

project (ibv_time_rnr C)

add_executable (ibv_time_rnr "ibv_time_rnr.c")
target_link_libraries (ibv_time_rnr ibverbs rt)