# Build the floor_it program

project (floor_it C)
cmake_minimum_required (VERSION 2.8)

add_executable (floor_it "floor_it.c")
set_target_properties (floor_it PROPERTIES COMPILE_FLAGS "-mfma")
target_link_libraries (floor_it m)
