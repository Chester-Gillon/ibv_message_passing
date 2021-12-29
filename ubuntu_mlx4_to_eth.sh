#! /bin/bash
# Script for a ConnectX-2 VPI to switch the ports from Infiniband to Ethernet.
# @todo assumes single dual-port card and Ethernet interface names for a Ubuntu installation.

if [[ $(id -u) -ne 0 ]]
then
   echo "Please run as root"
   exit 1
fi

# Tell NetworkManager that the ConnectX-2 devices using the mlx4_core driver are unmanaged.
# Having the devices as unmanaged ensures no conflicts from NetworkManager perform the following:
# a. Attempting to use DHCP to allocate an address.
# b. Preventing IPv6 link-local addresses being allocated.
#
# This also means if the ConnectX-2 devices are Infiniband they are also unmanaged by NetworkManager
#
# The file created persists across reboots.
#
# @todo Doesn't check that are overriding any other [keyfile] sections in other NetworkManager
#       configuration files.
nm_dev_ignore_file=/etc/NetworkManager/conf.d/99-unmanaged-devices.conf
if [ ! -f ${nm_dev_ignore_file} ]
then
   cat << EOF >> ${nm_dev_ignore_file}
[keyfile]
unmanaged-devices=driver:mlx4_core
EOF
   echo "Restarting Network Manager to take note of the unmanaged devices"
   systemctl restart NetworkManager
fi

# Remove the existing driver modules
rmmod mlx4_en mlx4_ib mlx4_core

# Reload the driver module, specifying the port type as Ethernet (2)
modprobe mlx4_core port_type_array=2,2

# The names of the Ethernet devices for each of the ports on the ConnectX-2 VPI card enumerate as.
mlx4_ethernet_devices="ens5 ens5d1"

for dev in ${mlx4_ethernet_devices}
do
   # Poll for the device to be created, as may be a delay after the driver module is loaded before the device appears
   echo "Waiting for ${dev} to be created"
   until [ -d /sys/class/net/${dev} ]
   do
      sleep 1
   done

   # Bring the device up. No specific IP address is set, but will have a IPv6 link-scope address automatically allocated
   ip link set ${dev} up

   # Set the MTU to the maximum supported by the T1700G-28TQ switch.
   ip link set ${dev} mtu 9216

   # Wait for IPv6 duplicate address detection to complete
   echo "Waiting for ${dev} duplicate address detection to complete"
   while [[ `ip -6 addr show ${dev} | grep -c "scope link"` -eq "0" ]]
   do
      sleep 1
   done
   until [[ `ip -6 addr show ${dev} | grep -c "tentative"` -eq "0" ]]
   do
      sleep 1
   done
done

