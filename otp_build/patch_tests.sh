#!/bin/bash
set -e

patch --verbose -o bs_match_SUITE_patched.erl ../otp/lib/compiler/test/bs_match_SUITE.erl bs_match_SUITE.erl.patch

