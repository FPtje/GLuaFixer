{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, pinNixDown ? true}:
let
  haskellOverlay = final: previous: {
    # Requires latest version to compile with GHC 8.8
    # https://github.com/UU-ComputerScience/uuagc/pull/2
    uuagc-cabal = previous.uuagc-cabal.overrideAttrs (old: rec {
      pname = "uuagc-cabal";
      name = "${pname}-${version}";
      version = "1.2.0.0";
      src = pkgs.fetchurl {
        url = "mirror://hackage/${pname}-${version}.tar.gz";
        inherit sha256;
      };
      sha256 = "1hb567h06zdhrrysb99d0c63ih5p1ii2jl0ng2l304w71m35pk2z";
    });
    # Same as uuagc-cabal
    uuagc = previous.uuagc.overrideAttrs (old: rec {
      pname = "uuagc";
      name = "${pname}-${version}";
      version = "0.9.53.1";
      src = pkgs.fetchurl {
        url = "mirror://hackage/${pname}-${version}.tar.gz";
        inherit sha256;
      };
      sha256 = "0agmvc1ng1dpnl0z8njilc2r51rgl9fh3lmxgprwc0y5dqqrn6zr";
    });
  };
  overlay = final: previous: {
    haskellPackages = previous.haskellPackages.extend haskellOverlay;
  };

  # Pin nixpkgs version down by default, but allow building with another version
  nixpkgs = if !pinNixDown then pkgs else import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "38eaa62f28384bc5f6c394e2a99bd6a4913fc71f";
    sha256 = "1pvbhvy6m5zmhhifk66ll07fnwvwnl9rrif03i4yc34s4f48m7ld";
  }) { overlays = [overlay]; };

  drv = nixpkgs.haskellPackages.callPackage ./. {};
in
if pkgs.lib.inNixShell then drv.env else drv
