# Build the ibv_write_index_rdma_ordering program

project (ibv_write_index_rdma_ordering C)
cmake_minimum_required (VERSION 2.8)

add_executable (ibv_write_index_rdma_ordering "ibv_write_index_rdma_ordering_main.c")
target_link_libraries (ibv_write_index_rdma_ordering ibverbs pthread)