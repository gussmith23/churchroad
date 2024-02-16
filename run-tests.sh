#!/bin/bash

set -e

cargo test

./yosys-plugin/run_tests.sh