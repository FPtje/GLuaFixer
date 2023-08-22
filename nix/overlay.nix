final: previous:
{
  glualintPkgs = {
    haskellOverlay = final.callPackage ./haskell-overlay.nix {};

    haskellPackages =
      previous.haskell.packages.ghc94.extend final.glualintPkgs.haskellOverlay;

    staticHaskellPackages =
      previous.pkgsStatic.haskell.packages.ghc94.extend final.glualintPkgs.haskellOverlay;
  };
}
