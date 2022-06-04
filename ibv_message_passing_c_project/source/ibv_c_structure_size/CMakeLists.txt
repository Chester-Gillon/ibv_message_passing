# Build the ibv_c_structure_size program

project (ibv_c_structure_size C)

include_directories ("${CMAKE_SOURCE_DIR}/ibv_message_transport")

add_executable (ibv_c_structure_size "ibv_c_structure_size.c")

