# ibv_message_passing
Tests of using Infiniband to pass messages, built upon the verbs API

The tests have been run using:
- Two PCs, each with a Mellanox Technologies MT26428 [ConnectX VPI PCIe 2.0 5GT/s - IB QDR / 10GigE].
- The "master" PC runs Ubuntu 18.04.1 LTS with a 4.15.0-38-generic Kernel.
- The "slave" PC runs Ubuntu 16.04.5 LTS with a 4.4.0-138-generic Kernel.
- An 8 port Infiniscale-IV Mellanox Technologies switch.
- Two QDR cables from each MT26428 to the switch.
- opensm as the subnet manager started as a service, running on the "master" PC.
- GCC 7.3.1 to compile the C code (supplied with GNAT Community 2018).
- GNAT Community 2018 (20180524-73) to compile the C and Ada code.
- openslp to manage the Infiniband queue-pair connection.
- Ubuntu 16.04 had an OpenSLP service, which has been removed for Ubuntu 18.04.
  Therefore, for the Ubuntu 18.04 PC built and installed openslp-2.0.0 from source.

http://www.rdmamojo.com/2014/11/08/working-rdma-ubuntu was used for how to install support for RDMA on Ubuntu.


Loading of RDMA Kernel modules at system boot
=============================================

The RDMA Kernel modules are loaded at system boot by adding the following to /etc/modules

# RDMA stack modules
rdma_cm
ib_uverbs
rdma_ucm
ib_ucm
ib_umad
ib_ipoib

# RDMA devices low-level drivers
mlx4_ib
mlx4_en
iw_cxgb3
iw_cxgb4
iw_nes


/etc/rc.local start up changes
==============================

On the master PC running Ubuntu 18.04 /etc/rc.local is:
#! /bin/sh -e

# Give the user permission to access the performance counters
sudo chmod 666 /dev/infiniband/umad?

# Ubuntu 18.04 doesn't have an openslp package, so start the daemon built from the openslp source.
slpd

# Report success
exit 0

On the slave PC running Ubuntu 14.04 /etc/rc.local is:
#!/bin/sh -e
#
# rc.local
#
# This script is executed at the end of each multiuser runlevel.
# Make sure that the script will "exit 0" on success or any other
# value on error.
#
# In order to enable or disable this script just change the execution
# bits.
#
# By default this script does nothing.

# Give the user permission to access the performance counters
sudo chmod 666 /dev/infiniband/umad?

exit 0


/etc/security/limits.conf changes
=================================

All users are given unlimited locked memory in /etc/security/limits.conf, since the message buffers need to
be locked into memory:
* soft memlock unlimited
* hard memlock unlimited
root soft memlock unlimited
root hard memlock unlimited

