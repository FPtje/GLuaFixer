{
  description = "glualint - Linter and pretty printer for Garry's Mod's variant of Lua.";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      flake = true;
      # This is specifically pinned to be the same as the nixpkgs used by static-haskell-nix. That
      # ensures that the caches from nh2 can be reused.
      rev = "ede5282c487a1fd2de64303ba59adad6726f1225";
    };
    flake-utils.url = "github:numtide/flake-utils";
    static-haskell-nix = {
      url = "github:nh2/static-haskell-nix/fp/fix-callPackage-dynamic-executables";
      flake = false;
    };
  };

  nixConfig = {
    substituters = "https://cache.nixos.org/ https://glualint.cachix.org";
    trusted-public-keys =
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= glualint.cachix.org-1:wHSvobqyCtsGg5tvumXl2SKi998JKhEZbbQeupxHHeY=";
  };

  outputs = { self, nixpkgs, flake-utils, static-haskell-nix}: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (import ./nix/overlay.nix) ];
      };

      # Use the default.nix in static-haskell-nix/survey to build a statically linked glualint
      survey = import (static-haskell-nix + "/survey") {
        normalPkgs = pkgs;
        compiler = "ghc965";
      };
      staticHaskellPackages = survey.haskellPackages.extend (pkgs.callPackage nix/haskell-overlay.nix {});

      # Function to build a devshell for glualint
      mkDevShell = mkBuildInputs: pkgs.glualintPkgs.haskellPackages.shellFor {
        packages = p: [ p.glualint ];
        buildInputs = mkBuildInputs pkgs.glualintPkgs.haskellPackages;
      };
    in
    {
      overlays.glualint = (import ./nix/overlay.nix);

      packages = {
        glualint = pkgs.glualintPkgs.haskellPackages.glualint;
        glualint-static = staticHaskellPackages.glualint;
        default = self.packages.${system}.glualint-static;
      };

      apps.default = {
        type = "app";
        program = "${self.packages.${system}.glualint-static}/bin/glualint";
      };

      devShells = {
        default = mkDevShell (haskellPackages: with haskellPackages; [
          cabal-install
          pkgs.cachix
          cabal-fmt
          haskell-language-server
          fourmolu
        ]);

        # The CI shell is similar to the dev shell, but requires fewer packages. Leaving them out
        # prevents downloading them unnecessarily.
        ci = mkDevShell (haskellPackages: with haskellPackages; [
          cabal-install
          cabal-fmt
          fourmolu
        ]);
      };
    }
  );
}
