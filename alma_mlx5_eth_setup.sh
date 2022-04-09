#! /bin/bash
# Script for a ConnectX-4 Lx in a PC running AlmaLinux to configure the Ethernet ports for use

if [[ $(id -u) -ne 0 ]]
then
   echo "Please run as root"
   exit 1
fi

# Tell NetworkManager that the ConnectX-4 Lx devices using the mlx5_core driver are unmanaged.
# Having the devices as unmanaged ensures no conflicts from NetworkManager perform the following:
# a. Attempting to use DHCP to allocate an address.
# b. Preventing IPv6 link-local addresses being allocated.
#
# This also means if the ConnectX-4 Lx devices are Infiniband they are also unmanaged by NetworkManager
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
unmanaged-devices=driver:mlx5_core
EOF
   echo "Restarting Network Manager to take note of the unmanaged devices"
   systemctl restart NetworkManager
fi

# Find the name of the Ethernet devices which use the mlx5_core driver
ethernet_devices=`ls /sys/class/net`
mlx5_ethernet_devices=""
for dev in ${ethernet_devices}
do
   driver_name=`readlink /sys/class/net/${dev}/device/driver`
   if [ -n "${driver_name}" ]
   then
      driver_name=`basename ${driver_name}`
   fi
   if [ "${driver_name}" = "mlx5_core" ]
   then
      mlx5_ethernet_devices="${mlx5_ethernet_devices} ${dev}"
   fi
done

for dev in ${mlx5_ethernet_devices}
do
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
