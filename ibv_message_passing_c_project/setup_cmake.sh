#! /bin/bash
# Set up the cmake build environment under Linux, to a clean state

# Get the absolute path of the workspace root directory, which is the parent directory of this script.
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`
WORKSPACE_PATH=$(readlink -f ${SCRIPT_PATH}/..)

GCC_PATH=/opt/GNAT/2019/bin

platforms="Debug Release Coverage"
for platform in ${platforms}
do
   build_dir=${SCRIPT_PATH}/bin/${platform}
   rm -rf ${build_dir}
   mkdir -p ${build_dir}
   pushd ${build_dir}
   cmake -G "Unix Makefiles" -DPLATFORM_TYPE=${platform} ${SCRIPT_PATH}/source -DCMAKE_C_COMPILER=${GCC_PATH}/gcc -DCMAKE_CXX_COMPILER=${GCC_PATH}/g++
   popd
done

