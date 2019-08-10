#!/bin/bash
set -e

patch -o bs_match_SUITE_patched.erl ../otp/lib/compiler/test/bs_match_SUITE.erl bs_match_SUITE.erl.patch

