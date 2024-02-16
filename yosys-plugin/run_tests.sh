#!/bin/bash
# Each test file should have a corresponding .expect file. The .expect file
# should contain the expected output of the Lakeroad Yosys backend for the
# given test file, minus any comment lines or blank lines.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

lit -v "$SCRIPT_DIR"/tests
exit 0

# Old implementation below, can delete if we can actually use lit.
# YOSYS=$(realpath "$SCRIPT_DIR/../../yosys")

# function run_lakeroad_backend() {
#   $YOSYS -q -l "$(mktemp)" -p "read_verilog -sv $1; write_lakeroad"
# }

# failed=0

# for f in "$SCRIPT_DIR"/tests/*.sv ; do
#   # Fail if "$f".expect does not exist
#   if [ ! -f "$f".expect ]; then
#     echo "No expect file for $f"
#     failed=1
#     continue
#   fi

#   # Run Lakeroad Yosys backend
#   out=$(run_lakeroad_backend "$f")

#   # Check result, ignoring comment lines and blank lines.
#   if ! diff -I '^;' -B <(echo "$out") <(cat "$f".expect); then
#     echo "Test failed for $f"
#     failed=1
#   fi 
# done

# exit $failed