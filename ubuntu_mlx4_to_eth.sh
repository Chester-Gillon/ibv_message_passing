#! /bin/bash
# Quick hack for a ConnectX-2 VPI to switch the ports from Infiniband to Ethernet.
# @todo assumes single dual-port card and Ethernet interface names for a Ubuntu installation.

# Switch the ports to Ethernet
echo eth > /sys/module/mlx4_core/drivers/pci\:mlx4_core/00*/mlx4_port1
echo eth > /sys/module/mlx4_core/drivers/pci\:mlx4_core/00*/mlx4_port2

# Ensure the module for Ethernet is loaded. May have not been loaded during boot when the Connect-X2 defaults to Infiniband
modprobe mlx4_en

# Enable the Ethernet devices and add IP addresses.
# RoCE can operate without the IP addresses being set.
# Different subnets are used for each port, as operating with direct cables rather than a switch.
ifconfig ens5 up
ifconfig ens5d1 up
ifconfig ens5 192.169.0.3
ifconfig ens5d1 192.170.0.4
