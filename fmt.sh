#!/usr/bin/env bash

set -e
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

# Rust formatting.
cargo fmt --manifest-path "$SCRIPT_DIR"/Cargo.toml

# Python formatting.
black "$SCRIPT_DIR"/yosys-plugin/tests/lit.cfg
