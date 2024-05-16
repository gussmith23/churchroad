# Generates Churchroad source code from Verilog using Yosys.
#
# Usage: python3 from_verilog.py -verilog=<verilog_file> -output_path=<output_file>

import argparse
import subprocess
import os

if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    # Add arguments
    parser.add_argument("-verilog", help="Path of Verilog file to convert")
    parser.add_argument("-output_path", help="Path to output file")
    parser.add_argument("-top_level_module", help="Path to output file")

    args = parser.parse_args()

    if args.verilog is None:
        print("Error: Verilog file not specified")
        exit(1)

    if args.output_path is None:
        print("Error: Output file not specified")
        exit(1)

    if args.top_level_module is None:
        print("Error: Top level module not specified")
        exit(1)

    # get CHURCHROAD_DIR from environment variable
    CHURCHROAD_DIR = os.getenv("CHURCHROAD_DIR")

    if CHURCHROAD_DIR is None:
        print("Error: CHURCHROAD_DIR environment variable not set")
        exit(1)

    # Generate Churchroad source code from Verilog
    churchroad_code = subprocess.check_output(
        [
            "yosys",
            "-m",
            f"{CHURCHROAD_DIR}/yosys-plugin/churchroad.so",
            "-q",
            "-p",
            f"read_verilog -sv {args.verilog}; prep -top {args.top_level_module}; pmuxtree; write_lakeroad",
        ]
    )

    with open(args.output_path, "w") as f:
        f.write(str(churchroad_code.decode("utf-8")))

    print(f"Generated Churchroad source code at {args.output_path}")
