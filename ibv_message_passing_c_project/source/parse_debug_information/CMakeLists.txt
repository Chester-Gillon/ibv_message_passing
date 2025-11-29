# Build programs which parse debug information

project (parse_debug_information C)

if (EXISTS /usr/include/libdwarf)
    # The include directory in openSUSE Leap 15.5
    include_directories ("/usr/include/libdwarf")
endif()

add_executable (list_enumerations_in_exe "list_enumerations_in_exe.c")
target_link_libraries (list_enumerations_in_exe dw)

add_executable (list_structures_in_exe "list_structures_in_exe.c")
target_link_libraries (list_structures_in_exe dw)
