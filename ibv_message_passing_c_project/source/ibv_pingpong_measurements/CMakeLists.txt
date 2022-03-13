# Build the ibv_pingpong_measurements program

project (ibv_pingpong_measurements C)
cmake_minimum_required (VERSION 2.9)

if (AARCH64_SYSROOT)
   include_directories("${AARCH64_SYSROOT}/glib-2.0/usr/include/glib-2.0"
                       "${AARCH64_SYSROOT}/glib-2.0/usr/lib/glib-2.0/include")
   link_directories("${AARCH64_SYSROOT}/glib-2.0/usr/lib")
else()
    include_directories ("/usr/include/glib-2.0")
    if (EXISTS /usr/lib/x86_64-linux-gnu/glib-2.0/include)
        # The configuration directory in Ubuntu 18.04
        include_directories ("/usr/lib/x86_64-linux-gnu/glib-2.0/include")
    elseif (EXISTS /usr/lib64/glib-2.0/include)
        # The configuration directory in AlmaLinux 8.5
        include_directories ("/usr/lib64/glib-2.0/include")
    else()
        message ("Can't identify glib2 configuration directory")
    endif()
endif()

# Can't compile under CentOS 6 as the glib2 version doesn't support the Unix specific functions
if (EXISTS /usr/include/glib-2.0/glib-unix.h)
    add_executable (ibv_pingpong_measurements "ibv_pingpong_measurements_main.c")

    target_link_libraries (ibv_pingpong_measurements glib-2.0)
endif()
