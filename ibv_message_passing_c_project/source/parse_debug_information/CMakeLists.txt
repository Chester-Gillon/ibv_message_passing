# Build programs which parse debug information

project (parse_debug_information C)
cmake_minimum_required (VERSION 2.8)

add_executable (list_enumerations_in_exe "list_enumerations_in_exe.c")
target_link_libraries (list_enumerations_in_exe dw)
