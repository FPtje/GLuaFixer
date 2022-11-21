#!/usr/bin/env bash

# This script builds an aarch64-linux version of GLuaLint. This script is
# designed to be called from a non-aarch64-linux device, to contact an
# aarch64-linux device to do the actual building.
#
# Personally (FPtje) I use this from my laptop to build glualint on my raspberry
# pi, and then get the resulting build back.
#
# Usage: ./release-aarch64-linux.sh <IP/hostname of your build host> E.g.
# ./release-aarch64-linux.sh 192.168.1.102
#
# This script assumes that the remote host has Nix installed, and that the local
# user has login rights, and Nix build rights.
#
# Note: It is recommended to do a manual initial build of glualint in a tmux,
# using the DRV_PATH from below, and running:
#
# nix build --print-build-logs --keep-going $DRV_PATH
#
# That way you can see the progress of downloading all the dependencies, and
# building some more

set -o errexit \
    -o nounset \
    -o pipefail


DRV_PATH=$(nix path-info --derivation .#packages.aarch64-linux.glualint-static)

BUILD_HOST=$1
nix copy --derivation --to "ssh://$BUILD_HOST" "$DRV_PATH"
echo "Copied $DRV_PATH to $BUILD_HOST"
OUT_PATH=$(ssh "$BUILD_HOST" "nix build --print-build-logs --print-out-paths $DRV_PATH")
VERSION=$(ssh "$BUILD_HOST" $OUT_PATH/bin/glualint --version)
scp "$BUILD_HOST:$OUT_PATH/bin/glualint" ./glualint
zip "glualint-$VERSION-aarch64-linux.zip" glualint
rm -f glualint
