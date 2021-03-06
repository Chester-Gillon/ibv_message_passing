#! /bin/bash
# Quick hack for a ConnectX-2 VPI to switch the ports from Infiniband to Ethernet.
# @todo assumes single dual-port card and Ethernet interface names for a CentOS 6 installation.

# Need to stop the ibacm server as otherwise unable to change the link type
service ibacm stop

# Switch the ports to Ethernet
echo eth > /sys/module/mlx4_core/drivers/pci\:mlx4_core/00*/mlx4_port1
echo eth > /sys/module/mlx4_core/drivers/pci\:mlx4_core/00*/mlx4_port2

# Ensure the module for Ethernet is loaded. May have not been loaded during boot when the Connect-X2 defaults to Infiniband
modprobe mlx4_en
sleep 2

# Enable the Ethernet devices and add IP addresses.
# RoCE can operate without the IP addresses being set.
# Different subnets are used for each port, as operating with direct cables rather than a switch.
# The expression to set the IP address from the dev_id is due to indeterminate behavioral about which eth1/eth2 device is which port
ifnums="1 2"
for if in ${ifnums}
do
   dev_id=$(< /sys/class/net/eth${if}/dev_id)
   ifconfig eth${if} up
   ifconfig eth${if} 192.$((169 + ${dev_id})).0.$((1 + ${dev_id}))
done

