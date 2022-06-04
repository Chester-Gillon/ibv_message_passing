# Build the gdb_unblock_sigchld program

project (gdb_unblock_sigchld C)

add_executable (gdb_unblock_sigchld "gdb_unblock_sigchld.c")

target_link_libraries (gdb_unblock_sigchld pthread)
