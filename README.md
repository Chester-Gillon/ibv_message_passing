# ibv_message_parsing
Tests of using Infiniband to pass messages, built upon the verbs API

The tests have been run using:
- A Mellanox Technologies MT26428 [ConnectX VPI PCIe 2.0 5GT/s - IB QDR / 10GigE].
- A cable looping back the two ports of the MT26428.
- opensm as the subnet manager.
- Ubuntu 16.04 LTS with a 4.4.0-92-generic Kernel.

http://www.rdmamojo.com/2014/11/08/working-rdma-ubuntu was used for how to install support for RDMA on Ubuntu.

The following was run on the shell after each boot to prepare for the tests:
$ ./rdma_start.sh
$ sudo opensm&
$ ulimit -l unlimited

The rdma_start.sh script loads the RDMA Kernel modules, since is not performed by any Ubuntu service.
opensm is used to start the subnet manager, since there is no managed switch and only one machine with the ports looped-back.
unlimited locked memory is needed, since the message buffers need to be locked into memory.

