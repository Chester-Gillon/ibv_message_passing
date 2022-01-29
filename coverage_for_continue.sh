#! /bin/bash
# @file collect_coverage.sh
# @date 20 Jan 2022
# @author Chester Gillon
# @details Compile, run and collect coverage for the coverage_for_continue using different versions of GNAT

# Get the absolute path of this script
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`

# Create a directory to store the executables and coverage results created by this script.
# This is done since are testing different versions of GNAT rather than using CMake which is setup for a single version.
RESULTS_ROOT=${SCRIPT_PATH}/ibv_message_passing_c_project/bin/coverage_for_continue
rm -rf ${RESULTS_ROOT}
mkdir ${RESULTS_ROOT}

SOURCE_PATHNAME=${SCRIPT_PATH}/ibv_message_passing_c_project/source/coverage_for_continue/coverage_for_continue.c
ORIGINAL_PATH=${PATH}

GNAT_VERSIONS="2017 2018 2019 2020 2021"
for GNAT_VERSION in ${GNAT_VERSIONS}
do
    GNAT_BIN=/opt/GNAT/${GNAT_VERSION}/bin

    if [ -x ${GNAT_BIN}/gcc ]
    then
        # Put tested GNAT version first in the path
        export PATH=${GNAT_BIN}:${ORIGINAL_PATH}
        GCC_VERSION=`gcc --version | grep GNAT`
        echo ""
        echo "Testing ${GCC_VERSION}"

        # Use a different results sub-directory for every GNAT version tested
        RESULTS_DIR=${RESULTS_ROOT}/${GNAT_VERSION}
        mkdir ${RESULTS_DIR}
        pushd ${RESULTS_DIR} > /dev/null

        # Compile the test program for coverage
        gcc ${SOURCE_PATHNAME} --coverage -o coverage_for_continue

        # Run the test program, which simply writes the coverage information
        ./coverage_for_continue

        # Collect the coverage results
        lcov -d . -c -o lcov.trace --rc lcov_branch_coverage=1 

        # Generate HTML report
        genhtml -o coverage_results lcov.trace --branch-coverage

        popd > /dev/null
    fi
done

PATH=${ORIGINAL_PATH}