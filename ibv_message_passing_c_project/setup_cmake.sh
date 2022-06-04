#! /bin/bash
# Set up the cmake build environment under Linux, to a clean state

# Get the absolute path of the workspace root directory, which is the parent directory of this script.
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`
WORKSPACE_PATH=$(readlink -f ${SCRIPT_PATH}/..)

# Prefer GNAT Community edition 2021, but since that can't be run on CentOS 6 due to glibc dependencies fall back
# to 2020 if the later version isn't installed.
GCC_PATH=/opt/GNAT/2021/bin
if [ ! -d ${GCC_PATH} ]
then
    GCC_PATH=/opt/GNAT/2020/bin
fi

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

# If the mingw32 compiler is available, create a cross-compiled platform for it.
# Specifying CMAKE_SYSTEM_NAME prevents the CMake test of the compiler reporting an error due to the compiler
# not supporting the -rdynamic command line option.
#
# The value of CMAKE_SYSTEM_NAME is adjusted based upon the major CMake version, based upon tests on different systems:
# a. With Cmake 2.8.12.2 and i686-w64-mingw32-gcc 4.9.2 under CentOS 6 Generic is required to suppress errors, and leaves
#    one warning about the shared library in ibv_message_transport being converted into a static library.
#    That shared library isn't required to be built for win32
# b. With CMake 3.10.2 and i686-w64-mingw32-gcc 7.3-win32 under Ubuntu 18.04 Windows can be used, and no warnings are
#    reported during the configuration.
#
# @todo If the need to change the CMAKE_SYSTEM_NAME is due to the CMake and/or mingw32 versions.
CMAKE_VERSION=`cmake --version | grep version | cut -d ' ' -f 3`
CMAKE_MAJOR=`echo ${CMAKE_VERSION} | cut -d '.' -f 1`
if [ ${CMAKE_MAJOR} -gt 2 ]
then
    SYSTEM_NAME=Windows
else
    SYSTEM_NAME=Generic
fi
echo "Selecting ${SYSTEM_NAME} as the CMake system name for the win32 platform"
MINGW_PREFIX=i686-w64-mingw32-
if [ -n "`which ${MINGW_PREFIX}gcc`" ]
then
   platforms="win32"
   for platform in ${platforms}
   do
      build_dir=${SCRIPT_PATH}/bin/${platform}
      rm -rf ${build_dir}
      mkdir -p ${build_dir}
      pushd ${build_dir}
      cmake -G "Unix Makefiles" -DPLATFORM_TYPE=${platform} ${SCRIPT_PATH}/source -DCMAKE_C_COMPILER=${MINGW_PREFIX}gcc -DCMAKE_CXX_COMPILER=${MINGW_PREFIX}g++ -DCMAKE_SYSTEM_NAME=${SYSTEM_NAME}
      popd
   done
fi
