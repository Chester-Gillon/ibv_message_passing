# ibv_message_passing
Tests of using Infiniband to pass messages, built upon the verbs API

The tests have been run using:
- A Mellanox Technologies MT26428 [ConnectX VPI PCIe 2.0 5GT/s - IB QDR / 10GigE].
- A cable looping back the two ports of the MT26428.
- opensm as the subnet manager.
- Ubuntu 16.04 LTS with a 4.4.0-92-generic Kernel.

@todo After an automatic update of the Kernel version to 4.4.0-93-generic seemed to break Infiniband.
The symptom was that calls to ibv_modify_qp to attempt to transition a Queue Pair from IBV_QPS_INIT to IBV_QPS_RTR failed with EINVAL.
By stepping the user mode libibverbs code the EINVAL occurs after an an attempt to write to the mlx4_core Kernel module.
ibv_modify_qp was failing in the ibv_rc_pingpong and ib_write_bw pre-compiled examples as well as from the ibv_functional_loopback_test program under development.
Reverting to booting 4.4.0-92-generic stopped the errors. Haven't attempted to compare the Infiniband driver changes between 4.4.0-92-generic and 4.4.0-93-generic

http://www.rdmamojo.com/2014/11/08/working-rdma-ubuntu was used for how to install support for RDMA on Ubuntu.

The following was run on the shell after each boot to prepare for the tests:
$ ./rdma_start.sh
$ sudo opensm&
$ ulimit -l unlimited

The rdma_start.sh script loads the RDMA Kernel modules, since is not performed by any Ubuntu service.
opensm is used to start the subnet manager, since there is no managed switch and only one machine with the ports looped-back.
unlimited locked memory is needed, since the message buffers need to be locked into memory.

