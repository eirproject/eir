#!/bin/bash
set -e

cp ../otp/lib/compiler/test/bs_match_SUITE.erl .
patch -l --normal --verbose bs_match_SUITE.erl bs_match_SUITE.erl.patch

