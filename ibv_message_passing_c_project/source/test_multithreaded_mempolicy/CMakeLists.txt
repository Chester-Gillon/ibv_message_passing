# Build the test_multithreaded_mempolicy program

project (test_multithreaded_mempolicy C)

add_executable (test_multithreaded_mempolicy "test_multithreaded_mempolicy_main.c")

target_link_libraries (test_multithreaded_mempolicy numa pthread)
