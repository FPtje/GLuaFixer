#!/bin/bash
set -o errexit \
    -o nounset \
    -o pipefail

nix build

VERSION=$(result/bin/glualint --version)
echo "Packing glualint version $VERSION"

cp result/bin/glualint .

zip "glualint-$VERSION-linux.zip" glualint

rm -f glualint
