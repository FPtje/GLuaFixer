#!/bin/bash
set -o errexit \
    -o nounset \
    -o pipefail

cabal clean

nix develop --command bash -c "./AGGenerator.sh && cabal v1-clean && cabal v1-build"

VERSION=$(dist/build/glualint/glualint --version)
echo "Packing glualint version $VERSION"

cp dist/build/glualint/glualint .

# See https://github.com/FPtje/GLuaFixer/issues/54
nix develop --command patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 glualint

zip "glualint-$VERSION-linux.zip" glualint

strip glualint

zip "glualint-$VERSION-linux-stripped.zip" glualint

rm glualint
