{ fetchFromGitHub }:
final: previous:
let
  # UUAGC overrides can likely be removed when UUAGC is updated. See
  # https://github.com/UU-ComputerScience/uuagc/pull/11
  uuagc-source = fetchFromGitHub
    {
      owner = "FPtje";
      repo = "uuagc";
      rev = "93783365b8e0771b092b9d99264227ca7c5d0823";
      sha256 = "sha256-hvJ/GShwR1AvSofPmmR4FJMPUwUaCMiL1l1CKcKvQw0=";
    };
in
{
  glualint = final.callPackage ../default.nix {};

  uuagc-cabal =
    final.callCabal2nixWithOptions
      "uuagc-cabal"
      uuagc-source
      "--subpath uuagc/trunk/cabal-plugin"
      { };

  uuagc =
    final.callCabal2nixWithOptions
      "uuagc-cabal"
      uuagc-source
      "--subpath uuagc/trunk"
      { uuagc-cabal = final.uuagc-cabal; };
}
