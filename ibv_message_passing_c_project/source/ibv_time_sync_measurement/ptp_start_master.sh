#! /bin/bash
# Start PTP as master

sudo -b nohup ptp4l -i eno1 > /dev/null
sudo -b nohup sudo phc2sys -s /dev/ptp0 -r -w > /dev/null
