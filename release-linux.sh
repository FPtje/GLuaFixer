#!/usr/bin/env bash
set -o errexit \
    -o nounset \
    -o pipefail

if [ -z ${CACHIX_AUTH_TOKEN+x} ]; then
    echo "Please set the CACHIX_AUTH_TOKEN"
    exit 1
fi;

nix build --print-build-logs --print-out-paths

VERSION=$(result/bin/glualint --version)
echo "Packing glualint version $VERSION"

cp result/bin/glualint .

zip "glualint-$VERSION-x86_64-linux.zip" glualint

rm -f glualint
cachix push glualint "$(readlink result)"
