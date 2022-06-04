# Build the programs which measure time synchronisation between PCs by transferring timestamps over Infiniband

project (ibv_time_sync_measurement C)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_library (ibv_time_sync_measurement_messages "ibv_time_sync_measurement_messages.c")

add_executable (ibv_time_sync_measurement_master "ibv_time_sync_measurement_master.c")
target_link_libraries (ibv_time_sync_measurement_master ibv_time_sync_measurement_messages
                       ibv_message_transport ibverbs rt slp)

add_executable (ibv_time_sync_measurement_slave "ibv_time_sync_measurement_slave.c")
target_link_libraries (ibv_time_sync_measurement_slave ibv_time_sync_measurement_messages
                       ibv_message_transport ibverbs rt slp)
