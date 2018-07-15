#! /bin/bash
# @file collect_coverage.sh
# @date 17 June 2018
# @author Chester Gillon
# @details Collect coverage results for the C and Ada programs

# Delete any previous coverage data
find . -name '*.gcda' | xargs rm

# Run a test of programs without any explict if's
ibv_message_passing_ada_project/obj/Coverage/ibv_ada_structure_size/ibv_ada_structure_size > ibv_message_passing_ada_project/source/ibv_ada_structure_size/ibv_ada_structure_size.csv
ibv_message_passing_ada_project/obj/Coverage/ibv_message_overlay_gdb_test/ibv_message_overlay_gdb_test

# Run the test with three worker C processes and one controller Ada process
ibv_message_passing_c_project/bin/Coverage/ibv_worker_process/ibv_worker_process 1 &
ibv_message_passing_c_project/bin/Coverage/ibv_worker_process/ibv_worker_process 2 &
ibv_message_passing_c_project/bin/Coverage/ibv_worker_process/ibv_worker_process 3 &
ibv_message_passing_ada_project/obj/Coverage/ibv_controller_process/ibv_controller_process_main &

# Wait for processes to exit
for job in `jobs -p`
do
echo $job
    wait $job
done

# Run the test for two instances of the ibv_message_bw program, each of which has two threads
ibv_message_passing_c_project/bin/Coverage/ibv_message_bw/ibv_message_bw --ib-dev=mlx4_0,mlx4_0 --ib-port=1,2 --thread=rx:0,rx:1 &
ibv_message_passing_c_project/bin/Coverage/ibv_message_bw/ibv_message_bw --ib-dev=mlx4_0,mlx4_0 --ib-port=2,1 --thread=tx:0,tx:1 --all-sizes=1 &

# Wait for processes to exit
for job in `jobs -p`
do
echo $job
    wait $job
done

# Collect the coverage results
mkdir -p coverage_results
lcov -d . -c -o coverage_results/lcov.trace --rc lcov_branch_coverage=1 > coverage_results/report.log

# Generate HTML report
genhtml -o coverage_results coverage_results/lcov.trace --branch-coverage >> coverage_results/report.log

