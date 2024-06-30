{ fetchFromGitHub }:
final: previous:
let
  # UUAGC overrides can likely be removed when UUAGC is updated. See
  # https://github.com/UU-ComputerScience/uuagc/pull/11
  uuagc-source = fetchFromGitHub
    {
      owner = "UU-ComputerScience";
      repo = "uuagc";
      rev = "7d1dcacb18846820f9f56bbf94209fd1c630fb97";
      sha256 = "sha256-TbJbSRvGG/U5SCLgVICx82d5g2m42rZGFwuT4pX6a30=";
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
