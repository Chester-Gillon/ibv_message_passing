# Build the vector_test_load program

project (vector_test_load C)
cmake_minimum_required (VERSION 2.9)

add_executable (vector_test_load "vector_test_load_main.c")
set_target_properties (vector_test_load PROPERTIES COMPILE_FLAGS "-mfma")
target_link_libraries (vector_test_load numa pthread rt)
