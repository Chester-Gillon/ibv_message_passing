# Build the read_ptp program

project (read_ptp C)
cmake_minimum_required (VERSION 2.9)

include_directories ("${LINUXPTP_ROOT}") 

add_executable (read_ptp "read_ptp.c" "${LINUXPTP_ROOT}/phc.c")
