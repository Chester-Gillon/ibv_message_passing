#! /bin/bash
# Add a set of VLANs to one Ethernet interface, which is for investigating getting the switch test program
# from https://github.com/Chester-Gillon/switch_test to run with RDMA rather than using raw Ethernet frames and PCAP.

eth_dev=$1
mac_offset=$2
if [ "${mac_offset}" == "" ]
then
   echo "Usage: $0 <ethernet_device_to_add_VLANs_to> <MAC_offset>"
   exit 1
fi

if [ ! -d /sys/class/net/${eth_dev} ]
then
   echo "${eth_dev} is not an Ethernet device"
   exit 1
fi

if [[ $(id -u) -ne 0 ]]
then
   echo "Please run as root"
   exit 1
fi

# Iterate over all ports used on the switch under test
port_num=1
while ((port_num <= 48))
do
   vlan_id=$((1000 + port_num))

   # Allocate the VLAN for the switch port
   vlan_dev=${eth_dev}.${vlan_id}
   ip link add link ${eth_dev} name ${vlan_dev} type vlan id ${vlan_id}

   # Allocate the MAC address used for the switch port
   ip link set ${vlan_dev} address 02:00:`printf "%02X" $((1 + mac_offset))`:00:00:`printf "%02X" ${port_num}`

   # Set the MTU to the Ethernet standard value. Otherwise the MTU inherits the value on the underlying Ethernet device,
   # which ubuntu_mlx4_to_eth.sh sets to a larger value.
   # @todo Commented out while investigating running IBV utils which pick the MTU from the underlying Ethernet device.
   #ip link set ${vlan_dev} mtu 1500

   # Bring the VLAN interface up. This allocates:
   # a. A IPv6 link-local address based on the MAC address used above.
   # b. A RoCE GID for the link-local address
   ip link set ${vlan_dev} up

   port_num=$((port_num + 1))
done

