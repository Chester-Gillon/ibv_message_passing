#! /bin/bash
# Use sudo to set capabilities to allow executables to run as a normal user

# Get the absolute path of this script.
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`

# Adjust the capabilities on an executable to match those required, either to add or remove capabilities.
# Only needs to take sudo action if the existing capabilities need to change.
function adjust_capabilities
{
    local executable=${SCRIPT_PATH}/bin/${platform}/$1
    local required_capabilities=$2

    # Only attempt to adjust capabilities on executables which exist, in case not all platforms have been built
    if [ -e ${executable} ]
    then
        if [ -z ${required_capabilities} ]
        then
            # Need to remove any existing capabilities
            local existing_capabilities=$(getcap ${executable})
            if [ -n "${existing_capabilities}" ]
            then
                echo "Removing all capabilities from ${executable}"
                sudo setcap -q -r ${executable}
            fi
        else
            # Need to set capabilities
            setcap -q -v ${required_capabilities} ${executable}
            local exit_status=$?
            if [ ${exit_status} -ne 0 ]
            then
                echo "Setting ${required_capabilities} on ${executable}"
                sudo setcap -q ${required_capabilities} ${executable}
            fi
        fi
    fi
}

# Iterate over all possible native and cross-compiled platforms the executables have been built for.
# The native x86_64 setcap can update capabilities on aarch64 executables.
platforms="debug release coverage aarch64_debug aarch64_release"
for platform in ${platforms}
do
    # For reading MSR registers
    adjust_capabilities read_smi_count/read_smi_count cap_sys_rawio=ep

    # To create raw Ethernet Queue-Pair
    adjust_capabilities ibv_switch_test/ibv_raw_packet_tx cap_net_raw=ep
    adjust_capabilities ibv_switch_test/ibv_raw_packet_switch_test cap_net_raw=ep
    # When using the mlx5 driver:
    # a. cap_net_raw is required to prevent attempting to open the device failing with:
    #     mlx5_common: DevX create TIS failed errno=121 status=0x3 syndrome=0x6a6678
    #     mlx5_net: Failed to create TIS 0/0 for [bonding] device rocep33s0f0.
    #     mlx5_net: TIS allocation failure
    # b. cap_net_admin is required to allow rte_eth_dev_get_module_info() and
    #    rte_eth_dev_get_module_eeprom() calls
    adjust_capabilities dpdk_switch_test/dpdk_information cap_net_raw,cap_net_admin=ep
    adjust_capabilities dpdk_switch_test/dpdk_switch_test cap_net_raw=ep
done
