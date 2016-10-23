{ mkDerivation, aeson, array, base, bytestring, containers
, directory, filemanip, filepath, ListLike, MissingH, mtl, parsec
, pretty, stdenv, uu-parsinglib, uuagc, uuagc-cabal, vector
}:
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
