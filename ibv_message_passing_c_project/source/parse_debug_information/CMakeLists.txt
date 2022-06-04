# Build programs which parse debug information

project (parse_debug_information C)

add_executable (list_enumerations_in_exe "list_enumerations_in_exe.c")
target_link_libraries (list_enumerations_in_exe dw)

add_executable (list_structures_in_exe "list_structures_in_exe.c")
target_link_libraries (list_structures_in_exe dw)
