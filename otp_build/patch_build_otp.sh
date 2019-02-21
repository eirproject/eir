#!/bin/bash
set -e

pushd ../otp

git reset --hard
git apply ../otp_build/otp_build_core.patch

./otp_build autoconf
./configure

# TODO: This will fail on the `tools` app. Will fix when needed
make preloaded libs

#APPS="kernel stdlib compiler common_test"
#for app_name in $APPS; do
#    make -C lib/$app_name opt
#done

popd
