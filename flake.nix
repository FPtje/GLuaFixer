{
  description = "glualint - Linter and pretty printer for Garry's Mod's variant of Lua.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      haskellOverlay = final: previous: with pkgs.haskell.lib; {
        uuagc-cabal = markUnbroken previous.uuagc-cabal;
      };
      haskellPackages = pkgs.haskell.packages.ghc90.extend haskellOverlay;
      staticHaskellPackages = pkgs.pkgsStatic.haskell.packages.ghc90.extend haskellOverlay;
    in {
      packages.glualint = haskellPackages.callPackage ./default.nix {};
      # This has to be about the ugliest hack I've ever written. Somehow the overlay in
      # staticHaskellPackages does not apply.
      packages.glualint-static = (staticHaskellPackages.callPackage ./default.nix {}).override {
        uuagc = staticHaskellPackages.uuagc.override {uuagc-cabal = staticHaskellPackages.uuagc-cabal;};
        uuagc-cabal = staticHaskellPackages.uuagc-cabal;
      };
      defaultPackage = self.packages.${system}.glualint-static;
      staticHaskellPackages=staticHaskellPackages;

      devShell = pkgs.mkShell {
        buildInputs = with haskellPackages; [
          cabal-install
          (ghcWithPackages (self: with self; [
            aeson array base bytestring containers directory filemanip filepath
            ListLike MissingH mtl optparse-applicative parsec pretty signal
            uu-parsinglib uuagc uuagc-cabal deepseq
          ]))
          haskell-language-server
        ];
      };
    }
  );
}
