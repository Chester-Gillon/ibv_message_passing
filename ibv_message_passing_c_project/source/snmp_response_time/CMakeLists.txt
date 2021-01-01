# Build the snmp_response_time program

project (snmp_response_time C)
cmake_minimum_required (VERSION 2.8)

add_executable (snmp_response_time "snmp_response_time.cpp")
if (AARCH64_SYSROOT)
   target_link_libraries (snmp_response_time rt netsnmp)
else()
   target_link_libraries (snmp_response_time rt snmp)
endif()
