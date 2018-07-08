# Build the gdb_unblock_sigchld program

project (display_kernel_threads CXX)
cmake_minimum_required (VERSION 2.8)

add_executable (gdb_unblock_sigchld "gdb_unblock_sigchld.c")

target_link_libraries (gdb_unblock_sigchld pthread)
