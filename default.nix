{ mkDerivation, aeson, array, base, bytestring, containers
, directory, deepseq, filemanip, filepath, ListLike, MissingH, mtl
, optparse-applicative, parsec, pretty, signal, stdenv, uu-parsinglib
, uuagc, uuagc-cabal, vector
, pkgs
}:
mkDerivation {
  pname = "glualint-lib";
  version = "0.1.0.0";
  src = ./.;
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
    ./AGGenerator.sh
  '';

  homepage = "https://github.com/FPtje/GLuaFixer";
  description = "Attempts to fix your syntax erroring Lua files";
  license = stdenv.lib.licenses.lgpl21;
}
