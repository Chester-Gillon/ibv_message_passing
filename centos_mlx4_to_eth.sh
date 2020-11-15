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

# Enable the Ethernet devices and add IP addresses.
# RoCE can operate without the IP addresses being set.
# Different subnets are used for each port, as operating with direct cables rather than a switch.
ifconfig eth1 up 192.169.0.1
ifconfig eth2 up 192.170.0.2
