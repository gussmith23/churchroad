#!/bin/bash
# Each test file should have a corresponding .expect file. The .expect file
# should contain the expected output of the Lakeroad Yosys backend for the
# given test file, minus any comment lines or blank lines.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

lit -v "$SCRIPT_DIR"/tests
