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

export YOSYS_COMMIT_HASH="70d35314dbd7521870047ed607897f22dc48cbc3"