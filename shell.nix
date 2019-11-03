{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, pinNixDown ? true}:
let
  # Pin nixpkgs version down by default, but allow building with another version
  nixpkgs = if !pinNixDown then pkgs else import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "c5aabb0d603e2c1ea05f5a93b3be82437f5ebf31";
    sha256 = "15fwszhn6078sbrb8qk83g8afvh4qnmvff0qbkbvq3cm1fxni2w1";
  }) {};

  drv = nixpkgs.haskellPackages.callPackage ./. {};
in
if pkgs.lib.inNixShell then drv.env else drv
