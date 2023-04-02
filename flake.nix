{
  description = "glualint - Linter and pretty printer for Garry's Mod's variant of Lua.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    substituters = "https://cache.nixos.org/ https://glualint.cachix.org";
    trusted-public-keys =
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= glualint.cachix.org-1:wHSvobqyCtsGg5tvumXl2SKi998JKhEZbbQeupxHHeY=";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.${system}.glualint ];
      };
    in
    {
      overlays.glualint = (import ./nix/overlay.nix);

      packages = {
        glualint = pkgs.glualintPkgs.haskellPackages.glualint;
        glualint-static = pkgs.glualintPkgs.staticHaskellPackages.glualint;
        default = self.packages.${system}.glualint-static;
      };

      apps.default = {
        type = "app";
        program = "${self.packages.${system}.glualint-static}/bin/glualint";
      };

      devShell = with pkgs.glualintPkgs.haskellPackages; shellFor {
        packages = p: [ p.glualint ];
        buildInputs = [
          cabal-install
          haskell-language-server
        ];
      };
    }
  );
}
