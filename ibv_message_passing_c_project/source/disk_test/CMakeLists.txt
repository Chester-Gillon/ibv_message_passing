# Build the test_test program

project (disk_test C)

CHECK_INCLUDE_FILE ("libudev.h" HAVE_LIBUDEV)

if (${HAVE_LIBUDEV})
    add_executable (disk_test "disk_test.c")
    target_compile_options (disk_test PRIVATE "-Wconversion")
    target_link_libraries (disk_test udev rt pthread)
 endif()