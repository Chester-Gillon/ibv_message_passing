# Build the ibv_time_dma_flush program

project (ibv_time_dma_flush C)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_time_dma_flush "ibv_time_dma_flush.c")
target_link_libraries (ibv_time_dma_flush ibv_message_latency_messages
                       ibv_message_transport ibverbs rt slp)
