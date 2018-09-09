#! /bin/bash
# @file measure_core_frequencies.sh
# @date 9 Sep 2018
# @author Chester Gillon
# @details Run the vector_test_load program with increasing number of SSE and AVX test threads,
#          to see how the core CPU frequency changes.
# @todo Has hard-coded core numbers, assuming running on a system with dual 6-core processors.

# Get the absolute path of the workspace root directory, which is a parent directory of this script.
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`
WORKSPACE_PATH=$(readlink -f ${SCRIPT_PATH}/../../..)

BIN_ROOT_PATH=${WORKSPACE_PATH}/ibv_message_passing_c_project/bin/Release
TEST_PROGRAM=${BIN_ROOT_PATH}/vector_test_load/vector_test_load

if [ ! -x ${TEST_PROGRAM} ]; then
   echo "Error: ${TEST_PROGRAM} does not exist"
   exit 1
fi

RESULTS_BASENAME=$1

if [ "${RESULTS_BASENAME}" == "" ]; then
    echo "Usage : <results_basename>"
    exit 1
fi

TEST_DURATION=20

${TEST_PROGRAM} -o ${BIN_ROOT_PATH}/${RESULTS_BASENAME} -d ${TEST_DURATION} -a 0 -s 6
${TEST_PROGRAM} -o ${BIN_ROOT_PATH}/${RESULTS_BASENAME} -d ${TEST_DURATION} -a 0-1 -s 6-7
${TEST_PROGRAM} -o ${BIN_ROOT_PATH}/${RESULTS_BASENAME} -d ${TEST_DURATION} -a 0-2 -s 6-8
${TEST_PROGRAM} -o ${BIN_ROOT_PATH}/${RESULTS_BASENAME} -d ${TEST_DURATION} -a 0-3 -s 6-9
${TEST_PROGRAM} -o ${BIN_ROOT_PATH}/${RESULTS_BASENAME} -d ${TEST_DURATION} -a 0-4 -s 6-10
${TEST_PROGRAM} -o ${BIN_ROOT_PATH}/${RESULTS_BASENAME} -d ${TEST_DURATION} -a 0-5 -s 6-11
