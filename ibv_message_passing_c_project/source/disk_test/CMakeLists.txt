# Build the test_test program

project (disk_test C)

add_executable (disk_test "disk_test.c")
target_compile_options (disk_test PRIVATE "-Wconversion")
target_link_libraries (disk_test rt pthread)