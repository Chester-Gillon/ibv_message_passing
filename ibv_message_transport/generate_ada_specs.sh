#!/bin/bash
# @file generate_ada_specs.sh
# @date 14 Jan 2018
# @author Chester Gillon
# @details Generate the Ada specification for the ibv_message_be_interface.h C include file.
#          This makes use of the g++ supplied with GNAT.
#          The Ada specifications are generated using in C++ mode to preserve the argument names,
#          but with an extern "C" block inside the ibv_message_bw_interface_ada.h to preserve
#          the C linkage naming.

DIRECTORY=$(cd `dirname $0` && pwd)
ADA_SPECS_DIR=${DIRECTORY}/ada_specs

GNAT_GPLUSPLUS=/usr/gnat/bin/g++

[ -d ${ADA_SPECS_DIR} ] || mkdir ${ADA_SPECS_DIR}
cd ${ADA_SPECS_DIR}

# Generate the ada spec.
#
# The standard is set to c++03, to disable the nullptr_t typedef in stddef.h.
# This is becuase the nullptr_t typedef enabled by the c++11 standard causes
# the generated ads spec to contain the following invalid ada:
#   subtype nullptr_t is ;  -- /usr/gnat/lib/gcc/x86_64-pc-linux-gnu/6.3.1/include/stddef.h:436
${GNAT_GPLUSPLUS} -fdump-ada-spec -std=c++03 ../ibv_message_bw_interface_ada.h

# The infiniband/verbs.h file has some structure and constant/enumeration names which only differ in case.
# The resulting generated ada spec is invalid due to conflicting declarations, as ada is case insensitive.
# Rename the conflicting constants/enumerations to make the ada spec valid.
sed -i 's/IBV_QP_CAP/IBV_QP_CAP_ENUM/g' infiniband_verbs_h.ads
sed -i 's/IBV_QP_STATE/IBV_QP_STATE_CONST/g' infiniband_verbs_h.ads
sed -i 's/IBV_FLOW_SPEC_ETH/IBV_FLOW_SPEC_ETH_CONST/g' infiniband_verbs_h.ads
sed -i 's/IBV_FLOW_SPEC_IPV4/IBV_FLOW_SPEC_IPV4_CONST/g' infiniband_verbs_h.ads
