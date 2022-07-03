#! /bin/bash
# @file coverage_for_ada_task.sh
# @date 28 May 2022
# @author Chester Gillon
# @details Compile, run and collect coverage for the coverage_for_ada_task using different versions of GNAT

# Get the absolute path of this script
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`

# Parse command line arguments
COVERAGE_ARGS=""
CLEAR_ARTIFICIAL_EXE=""
RTS_SELECTION=""
NO_EXCEPTION_BRANCH=""
while [ -n "${1}" ]
do
    case ${1}
    in
        --optional-coverage)
            COVERAGE_ARGS=${1}
        ;;
        --clear-artificial)
            CLEAR_ARTIFICIAL_EXE=${SCRIPT_PATH}/ibv_message_passing_c_project/bin/debug/clear_gcno_artificial_function_flags/clear_gcno_artificial_function_flags
            if [ ! -x ${CLEAR_ARTIFICIAL_EXE} ]
            then
                echo "${CLEAR_ARTIFICIAL_EXE} executable not found"
                exit 1
            fi
        ;;
        --rts-sjlj)
            RTS_SELECTION="--RTS=sjlj"
        ;;
        --no-exception-branch)
            # The ability to exclude branches for exceptions requires lcov 1.15 or later, which adds the configuration parameter
            NO_EXCEPTION_BRANCH="--rc geninfo_no_exception_branch=1"
        ;;
        *)
            echo "Unknown argument ${1}"
            exit 1
        ;;
    esac
    shift
done

# Create a directory to store the executables and coverage results created by this script.
# This is done since are testing different versions of GNAT rather than using CMake which is setup for a single version.
RESULTS_ROOT=${SCRIPT_PATH}/ibv_message_passing_ada_project/obj/coverage_for_ada_task
rm -rf ${RESULTS_ROOT}
mkdir ${RESULTS_ROOT}

SOURCE_PATHNAME=${SCRIPT_PATH}/ibv_message_passing_ada_project/source/coverage_for_ada_task/coverage_for_ada_task.adb
ORIGINAL_PATH=${PATH}

GNAT_VERSIONS="2017 2018 2019 2020 2021 gnat-x86_64-linux-11.2.0-4"
for GNAT_VERSION in ${GNAT_VERSIONS}
do
    GNAT_BIN=/opt/GNAT/${GNAT_VERSION}/bin

    if [ -x ${GNAT_BIN}/gcc ]
    then
        # Put tested GNAT version first in the path
        export PATH=${GNAT_BIN}:${ORIGINAL_PATH}
        GCC_VERSION=`gcc --version | grep GCC`
        echo ""
        echo "Testing ${GCC_VERSION}"

        # Use a different results sub-directory for every GNAT version tested
        RESULTS_DIR=${RESULTS_ROOT}/${GNAT_VERSION}
        mkdir ${RESULTS_DIR}
        pushd ${RESULTS_DIR} > /dev/null

        # Compile the test program for coverage. 
        # The -g for gnatlink stops the binder generated source file from being deleted, which overwise causes
        # the HTML report generation to fail with an error that the binder source file isn't found.
        gcc ${SOURCE_PATHNAME} -c --coverage -save-temps ${RTS_SELECTION}
        gnatbind -x coverage_for_ada_task ${RTS_SELECTION}
        gnatlink -g coverage_for_ada_task --coverage -M ${RTS_SELECTION}

        # When enabled by the command line options clear the artificial flags for functions in the gcno files to test
        # if that allows coverage to be reported for Ada tasks.
        if [ -n "${CLEAR_ARTIFICIAL_EXE}" ]
        then
            find . -name '*.gcno' -print0 | xargs -0 -n1 ${CLEAR_ARTIFICIAL_EXE}
        fi

        # Run the test program, which simply writes the coverage information
        ./coverage_for_ada_task
        if [ -n ${COVERAGE_ARGS} ]
        then
            # Run a second time with addtional branch coverage
            ./coverage_for_ada_task ${COVERAGE_ARGS}
        fi

        # Collect the coverage results
        lcov -d . -c -o lcov.trace --rc lcov_branch_coverage=1 ${NO_EXCEPTION_BRANCH} 

        # Generate HTML report
        genhtml -o coverage_results lcov.trace --branch-coverage

        # Use gcov to generate report just for the main source file, which uses the executable which is part of the
        # GNAT installation and doesn't use the lcov package which contains the lcov and genhtml perl scripts used above.
        gcov -a -c -b coverage_for_ada_task

        # Dump the coverage file contents for diagonstics
        gcov-dump -l coverage_for_ada_task.gcno > coverage_for_ada_task.gcno.dump
        gcov-dump -l coverage_for_ada_task.gcda > coverage_for_ada_task.gcda.dump

        popd > /dev/null
    fi
done

PATH=${ORIGINAL_PATH}