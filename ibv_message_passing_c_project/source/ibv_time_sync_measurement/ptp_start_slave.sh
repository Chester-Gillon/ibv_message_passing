#! /bin/bash
# Start PTP as slave

sudo -b nohup ptp4l -i eno1 -s > /dev/null
sudo -b nohup sudo phc2sys -s /dev/ptp0 -r -w -O 0 > /dev/null
