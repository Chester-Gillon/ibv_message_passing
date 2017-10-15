#! /bin/bash
# Called after the ibv_message_bw program has aborted with an error to remove resources, to allow
# another run of of the program

# Remove any POSIX shared memory files
rm /dev/shm/ibv_message_bw_*

# De-register any outstanding SLP service URLs
# @todo Even though slpd under Ubuntu 16.04 LTS should be configured to automatically de-register
#       services when the creating PID is no longer running, this is not happening
service_urls=`slptool findsrvs service:message_bw`
for service_url in ${service_urls} 
do
   service_url_no_lifetime=`echo "${service_url}" | sed "s/,[0-9]*$//"`
   slptool deregister "${service_url_no_lifetime}"
   echo "De-registered ${service_url_no_lifetime}"
done
