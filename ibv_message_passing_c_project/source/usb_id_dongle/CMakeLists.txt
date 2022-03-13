# Build the usb_id_dongle program

project (usb_id_dongle C)
cmake_minimum_required (VERSION 2.9)

add_executable (read_usb_id_dongle "read_usb_id_dongle.c")
if (AARCH64_SYSROOT)
   target_link_libraries (read_usb_id_dongle ftdi1)
else()
   if (EXISTS /usr/include/libftdi1/ftdi.h)
       # Include location and library on AlmaLinux 8.5
       include_directories("/usr/include/libftdi1")
       target_link_libraries (read_usb_id_dongle ftdi1)
   else()
       target_link_libraries (read_usb_id_dongle ftdi)
   endif()
endif()