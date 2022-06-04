# Build the read_ptp program

project (read_ptp C)

include_directories ("${LINUXPTP_ROOT}") 

add_executable (read_ptp "read_ptp.c" "${LINUXPTP_ROOT}/phc.c")
