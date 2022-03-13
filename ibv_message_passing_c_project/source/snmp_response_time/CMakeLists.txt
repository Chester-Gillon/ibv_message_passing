# Build the snmp_response_time program

project (snmp_response_time C)
cmake_minimum_required (VERSION 2.9)

add_executable (snmp_response_time "snmp_response_time.cpp")
if (AARCH64_SYSROOT)
   target_link_libraries (snmp_response_time rt netsnmp)
else()
   find_library(SNMP_LIB snmp)
   find_library(NETSNMP_LIB netsnmp)
   if (SNMP_LIB)
       # For Ubuntu
       target_link_libraries (snmp_response_time rt snmp)
   elseif (NETSNMP_LIB)
       # For AlmaLinux 8.5
       target_link_libraries (snmp_response_time rt netsnmp)
   endif()
endif()
