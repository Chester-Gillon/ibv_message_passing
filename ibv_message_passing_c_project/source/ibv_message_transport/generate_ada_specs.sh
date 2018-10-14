#!/bin/bash
# @file generate_ada_specs.sh
# @date 14 Jan 2018
# @author Chester Gillon
# @details Generate the Ada specifications for the ibv_message_be_interface.h and ibv_controller_worker_messages.h C include files.
#          This makes use of the g++ supplied with GNAT.
#          The Ada specifications are generated using in C++ mode to preserve the argument names,
#          but with an extern "C" block inside the ibv_message_bw_interface_ada.h to preserve
#          the C linkage naming.

# Get the absolute path of the workspace root directory, which is a parent directory of this script.
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`
WORKSPACE_PATH=$(readlink -f ${SCRIPT_PATH}/../../..)

ADA_SPECS_DIR=${WORKSPACE_PATH}/ibv_message_passing_ada_project/source/ibv_message_transport

GNAT_GPLUSPLUS=/opt/GNAT/2018/bin/g++

[ -d ${ADA_SPECS_DIR} ] || mkdir ${ADA_SPECS_DIR}
cd ${ADA_SPECS_DIR}

# Generate the ada spec.
#
# The standard is set to c++03, to disable the nullptr_t typedef in stddef.h.
# This is becuase the nullptr_t typedef enabled by the c++11 standard causes
# the generated ads spec to contain the following invalid ada:
#   subtype nullptr_t is ;  -- /usr/gnat/lib/gcc/x86_64-pc-linux-gnu/6.3.1/include/stddef.h:436
${GNAT_GPLUSPLUS} -fdump-ada-spec -std=c++03 ${SCRIPT_PATH}/ibv_message_bw_interface_ada.h
${GNAT_GPLUSPLUS} -fdump-ada-spec -std=c++03 ${SCRIPT_PATH}/ibv_controller_worker_messages_ada.h

# The infiniband/verbs.h file has some structure and constant/enumeration names which only differ in case.
# The resulting generated ada spec is invalid due to conflicting declarations, as ada is case insensitive.
# Rename the conflicting constants/enumerations to make the ada spec valid.
sed -i 's/IBV_QP_CAP/IBV_QP_CAP_ENUM/g' infiniband_verbs_h.ads
sed -i 's/IBV_QP_STATE/IBV_QP_STATE_CONST/g' infiniband_verbs_h.ads
sed -i 's/IBV_FLOW_SPEC_ETH/IBV_FLOW_SPEC_ETH_CONST/g' infiniband_verbs_h.ads
sed -i 's/IBV_FLOW_SPEC_IPV4/IBV_FLOW_SPEC_IPV4_CONST/g' infiniband_verbs_h.ads
sed -i 's/IBV_FLOW_SPEC_IPV6/IBV_FLOW_SPEC_IPV6_CONST/g' infiniband_verbs_h.ads
sed -i 's/IBV_FLOW_SPEC_ACTION_TAG/IBV_FLOW_SPEC_ACTION_TAG_CONST/g' infiniband_verbs_h.ads
sed -i 's/IBV_FLOW_SPEC_ACTION_DROP/IBV_FLOW_SPEC_ACTION_DROP_CONST/g' infiniband_verbs_h.ads

# -fdump-ada-spec inserts a pragma to select Ada 2005, but GNAT Community Edition 2018 no longer recognises switches or pragmas
# to select Ada2005 or earlier. Therefore, change to Ada 2012 to prevent warnings about an unrecognised pragma.
sed -i 's/pragma Ada_2005;/pragma Ada_2012;/g' *.ads
