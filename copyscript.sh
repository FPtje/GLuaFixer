#!/usr/bin/env /bin/bash

# The point of placing copies of these generated files near their source codes is making ghci and the linter work
# in files that include them.
rm src/GLua/AG/*.hs
cp dist/build/gluafixer/gluafixer-tmp/GLua/AG/*.hs src/GLua/AG/
