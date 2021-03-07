#!/bin/bash

# Kill any existing opensm process
sudo pkill --exact opensm

# Start opensm on all Infiniband ports
PORTS=`/usr/sbin/ibstat -p`
for PORT in ${PORTS} ;
do
   echo "Starting opensm on ${PORT}: "
   sudo /usr/sbin/opensm -g ${PORT} --qos --config opensm.conf -B -f /var/log/opensm.${PORT}.log
done
