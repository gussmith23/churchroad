#!/bin/sh
# This script exports a number of variables that help to pin the versions of
# various Churchroad dependencies.
#
# To use, source this script before running relevant commands e.g. in a
# Dockerfile.
#
# This is just one possible way to implement dependency tracking. Some of the
# other options are:
# - No tracking at all. E.g. in the Dockerfile, we could clone Yosys at a
#   specific commit, using a "magic constant" commit hash. This isn't ideal,
#   because if we use Yosys elsewhere (e.g. in the evaluation) we have to make
#   sure we keep the commit hashes in sync.
# - Git submodules. This is very similar to what we've chosen to do, but it's
#   more directly supported by Git. However, it's a bit overkill to add a full
#   repository as a submodule when we only need the resulting binary.
#
# This option is essentially a lighter-weight version of submodules. We track
# the commit hashes of the dependencies we need, but nothing additional is
# cloned on a `git clone --recursive`.

export YOSYS_COMMIT_HASH="aa30589c123844bc576fc3c9938157b5db35a7c1"
# ABC and CXXOPTS versions must match the submodule versions in the above Yosys
# version.
export ABC_COMMIT_HASH="e55d316cc9a7f72a84a76eda555aa6ec083c9d0d"
export CXXOPTS_COMMIT_HASH="4bf61f08697b110d9e3991864650a405b3dd515d"

export VERILATOR_COMMIT_HASH="522bead374d6b7b2adb316304126e5361b18bcf1"
