#! /bin/bash
# Set up the cmake build environment under Linux, to a clean state

# Get the absolute path of the workspace root directory, which is the parent directory of this script.
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`
WORKSPACE_PATH=$(readlink -f ${SCRIPT_PATH}/..)

GCC_PATH=/opt/GNAT/2021/bin

# Optional argument is the root of rdma-core build directory to build against instead of the system installed version.
# Allows testing of rdma-core changes without having to modify the system installed version
if [ -n "$1" ]
then
   LOCAL_RDMA_CORE_SELECT="-DLOCAL_RDMA_CORE_ROOT=$1"
fi

# Create the native platforms
platforms="debug release coverage"
for platform in ${platforms}
do
   build_dir=${SCRIPT_PATH}/bin/${platform}
   rm -rf ${build_dir}
   mkdir -p ${build_dir}
   pushd ${build_dir}
   cmake -G "Unix Makefiles" -DPLATFORM_TYPE=${platform} ${SCRIPT_PATH}/source -DCMAKE_C_COMPILER=${GCC_PATH}/gcc -DCMAKE_CXX_COMPILER=${GCC_PATH}/g++ ${LOCAL_RDMA_CORE_SELECT}
   popd
done

# Create the cross-compiled platforms
#
# @todo The AARCH64_SYSROOT depends upon the tmp build artifacts of a petalinux Project, which has different sub-directories
#       for each library. 
AARCH64_PATH=/opt/Xilinx/Vitis/2020.1/gnu/aarch64/lin/aarch64-linux/bin
AARCH64_PREFIX=${AARCH64_PATH}/aarch64-linux-gnu-
AARCH64_SYSROOT=/opt/pkg/petalinux/2020.1/MYD_CZU4EV_RC/build/tmp/sysroots-components/aarch64
if [[ -d ${AARCH64_PATH} && -d ${AARCH64_SYSROOT} ]]
then
   platforms="aarch64_debug aarch64_release"
   for platform in ${platforms}
   do
      build_dir=${SCRIPT_PATH}/bin/${platform}
      rm -rf ${build_dir}
      mkdir -p ${build_dir}
      pushd ${build_dir}
      cmake -G "Unix Makefiles" -DPLATFORM_TYPE=${platform} ${SCRIPT_PATH}/source -DCMAKE_C_COMPILER=${AARCH64_PREFIX}gcc -DCMAKE_CXX_COMPILER=${AARCH64_PREFIX}g++ -DAARCH64_SYSROOT=${AARCH64_SYSROOT}
      popd
   done
fi
