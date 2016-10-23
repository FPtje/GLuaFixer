{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv}:
let
  nixpkgs = import (pkgs.fetchgit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "af0fec6d0a3e28c815e38296f3758e7d0916eba9";
    sha256 = "0npkwybsh3v5jmjwp27xg47s0467msqilpiq2wikrmnrgb86ynpc";
  }) {};
in
with nixpkgs.haskellPackages;
mkDerivation {
  pname = "glualint-lib";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring containers directory filemanip filepath
    ListLike MissingH mtl parsec pretty uu-parsinglib uuagc uuagc-cabal
    vector
  ];
  executableHaskellDepends = [ base directory ];
  homepage = "https://github.com/FPtje/GLuaFixer";
  description = "Attempts to fix your syntax erroring Lua files";
  license = stdenv.lib.licenses.lgpl21;
}
