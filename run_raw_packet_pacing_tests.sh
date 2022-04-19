#! /bin/bash
# Run the ibv_raw_packet_tx executable under some different conditions to evaluate how packets are paced in "bursts".
# The script just generates CSV files containing the completion timestamps, which requires manual analysis to
# determine if packets are transmitted in bursts or not.

# Test executable in sub-directory of this script
test_exe=ibv_message_passing_c_project/bin/release/ibv_switch_test/ibv_raw_packet_tx
if [ ! -x ${test_exe} ]
then
   echo "Error: ${test_exe} not found"
   exit 1
fi

result_basename=$1
if [ -z "${result_basename}" ]
then
   echo "Usage: $0 <result_basname>"
   exit 1
fi
if [ ! -d `dirname ${result_basename}` ]
then
   echo "Error: The directory part of ${result_basename} doesn't exist" 
   exit 1
fi

# MTUs greater that the 1500 used by the test program can cause packets in bursts.
mtus="9216 2048 1500"
for mtu in ${mtus}
do
   # Set the requested MTU for the test
   sudo sudo ip link set enp1s0f0 mtu ${mtu}

   # Rates for exercising 2, 24 or 48 ports at 100Mbps per port
   rates_kbps="200000 1200000 2400000 4800000"
   for rate_kbps in ${rates_kbps}
   do
      set_burst_sizes="false true"
      for set_burst_size in ${set_burst_sizes} 
      do
         burst_option=""
         if [ "${set_burst_size}" == "true" ]
         then
            burst_option="-b"
         fi
         csv_pathname="${result_basename}_m${mtu}_l${rate_kbps}${burst_option}.csv"
         echo
         echo "${test_exe} -i rocep1s0f0 -n 1 -p 25-26 -l ${rate_kbps} -c ${csv_pathname} ${burst_option}"
         ${test_exe} -i rocep1s0f0 -n 1 -p 25-26 -l ${rate_kbps} -c ${csv_pathname} ${burst_option}
      done
   done
done
