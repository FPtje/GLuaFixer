{ mkDerivation, aeson, array, base, bytestring, containers
, directory, deepseq, filemanip, filepath, ListLike, MissingH, mtl
, optparse-applicative, parsec, pretty, signal, lib, uu-parsinglib
, uuagc, uuagc-cabal, vector
, pkgs
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
        ".sh"
        "LICENSE"
      ];
      suffixDenylist = [
        "release-linux.sh"
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
    aeson array base bytestring containers directory filemanip filepath
    ListLike MissingH mtl optparse-applicative parsec pretty signal
    uu-parsinglib uuagc uuagc-cabal deepseq

    # Require haskellPackages' uuagc and uuagc-cabal specifically
    # Otherwise compilation with ghcjs will fuck up.
    pkgs.haskellPackages.uuagc pkgs.haskellPackages.uuagc-cabal
    vector
  ];
  executableHaskellDepends = [ base directory ];

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
