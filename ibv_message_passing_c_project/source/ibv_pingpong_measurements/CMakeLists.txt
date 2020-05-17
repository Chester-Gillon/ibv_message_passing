# Build the ibv_pingpong_measurements program

project (ibv_pingpong_measurements C)
cmake_minimum_required (VERSION 2.8)

include_directories ("/usr/include/glib-2.0"
                     "/usr/lib/x86_64-linux-gnu/glib-2.0/include")

# Can't compile under CentOS 6 as the glib2 version doesn't support the Unix specific functions
if (EXISTS /usr/include/glib-2.0/glib-unix.h)
    add_executable (ibv_pingpong_measurements "ibv_pingpong_measurements_main.c")

    target_link_libraries (ibv_pingpong_measurements glib-2.0)
endif()
