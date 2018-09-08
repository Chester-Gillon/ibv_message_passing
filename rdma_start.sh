#!/bin/bash
# In Ubuntu there isn't any service file to load and unload the RDMA drivers; this needs to be done manually. 

# RDMA stack modules
sudo modprobe rdma_cm
sudo modprobe ib_uverbs
sudo modprobe rdma_ucm
sudo modprobe ib_ucm
sudo modprobe ib_umad
sudo modprobe ib_ipoib

# RDMA devices low-level drivers
sudo modprobe mlx4_ib
sudo modprobe mlx4_en
sudo modprobe iw_cxgb3
sudo modprobe iw_cxgb4
sudo modprobe iw_nes
sudo modprobe iw_c2

# Give the user permission to access the performance counters
sudo chmod 666 /dev/infiniband/umad?

# Start opensm on all Infiniband ports
PORTS=`/usr/sbin/ibstat -p`
for PORT in ${PORTS} ;
do
   echo "Starting opensm on ${PORT}: "
   sudo /usr/sbin/opensm -g ${PORT} -B -f /var/log/opensm.${PORT}.log
done
