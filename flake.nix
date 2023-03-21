{
  description = "glualint - Linter and pretty printer for Garry's Mod's variant of Lua.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      # This override can likely be removed when UUAGC is updated. See
      # https://github.com/UU-ComputerScience/uuagc/pull/11
      uuagc-source = pkgs.fetchFromGitHub
        { owner = "FPtje";
          repo = "uuagc";
          rev = "93783365b8e0771b092b9d99264227ca7c5d0823";
          sha256 = "sha256-hvJ/GShwR1AvSofPmmR4FJMPUwUaCMiL1l1CKcKvQw0=";
        };
      haskellOverlay = final: previous: with pkgs.haskell.lib; {
        uuagc-cabal = final.callCabal2nixWithOptions "uuagc-cabal" uuagc-source "--subpath uuagc/trunk/cabal-plugin" {};
        uuagc = final.callCabal2nixWithOptions "uuagc-cabal" uuagc-source "--subpath uuagc/trunk" {};
      };

      haskellPackages = pkgs.haskell.packages.ghc92.extend haskellOverlay;
      staticHaskellPackages = pkgs.pkgsStatic.haskell.packages.ghc92.extend haskellOverlay;
    in {
      asdf = haskellPackages;
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
