{ mkDerivation, aeson, base, bytestring, containers
, directory, deepseq, effectful, filemanip, filepath
, optparse-applicative, parsec, pretty, signal, lib, uu-parsinglib
, uuagc, uuagc-cabal
}:
let
# Clean up the source of the derivation to prevent rebuilds
src = lib.cleanSourceWith rec {
  name = "glualint-src";
  src = ./.;
  filter = path: type:
    let
      suffixAllowlist = [
        ".ag"
        ".cabal"
        ".hs"
        "LICENSE"
      ];
      suffixDenylist = [
        "release-linux.sh"
        "nix"
        "installation-instructions"
      ];
    in
      ((type == "directory") ||
       (builtins.any (suffix: lib.hasSuffix suffix path) suffixAllowlist)
      ) &&
      !(builtins.any (suffix: lib.hasSuffix suffix path) suffixDenylist) &&
      # Simple library function to remove git related files.
      lib.cleanSourceFilter path type
    ;
};

in mkDerivation {
  inherit src;

  pname = "glualint-lib";
  version = "0.1.0.0";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [uuagc uuagc-cabal];
  libraryHaskellDepends = [
    base
    aeson
    bytestring
    containers
    parsec
    pretty
    uu-parsinglib
    uuagc
    uuagc-cabal
  ];
  executableHaskellDepends = [
    aeson
    base
    bytestring
    containers
    deepseq
    directory
    effectful
    filemanip
    filepath
    optparse-applicative
    signal
  ];

  preBuild = ''
    printf "Generating attribute grammar haskell files"
    ${uuagc}/bin/uuagc --haskellsyntax --data src/GLua/AG/AST.ag
    ${uuagc}/bin/uuagc --haskellsyntax --data --strictdata src/GLua/AG/Token.ag
    ${uuagc}/bin/uuagc --catas --haskellsyntax --semfuns --wrappers --signatures src/GLua/AG/PrettyPrint.ag
    ${uuagc}/bin/uuagc --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/LexLint.ag
    ${uuagc}/bin/uuagc --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/ASTLint.ag
  '';

  homepage = "https://github.com/FPtje/GLuaFixer";
  description = "Attempts to fix your syntax erroring Lua files";
  license = lib.licenses.lgpl21;
}
