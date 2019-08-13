#!/bin/bash
set -e

cat bs_match_SUITE.erl.patch
cat ../otp/lib/compiler/test/bs_match_SUITE.erl

patch --normal --verbose -o bs_match_SUITE_patched.erl ../otp/lib/compiler/test/bs_match_SUITE.erl bs_match_SUITE.erl.patch

