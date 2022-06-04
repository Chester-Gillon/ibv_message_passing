# Build the log10f_test program

project (log10f_test C)

add_executable (log10f_test "log10f_test.c")
set_target_properties (log10f_test PROPERTIES COMPILE_FLAGS "-mfma")
target_link_libraries (log10f_test m)
