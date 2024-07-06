{ fetchFromGitHub }:
final: previous:
let
  # UUAGC overrides can likely be removed when UUAGC is updated. See
  # https://github.com/UU-ComputerScience/uuagc/pull/11
  uuagc-source = fetchFromGitHub
    {
      owner = "UU-ComputerScience";
      repo = "uuagc";
      rev = "e993213e5a4f09844863e536a9200dc0c046550d";
      sha256 = "sha256-Fso+t3Hm2oXZCcuOzCfRbooCtA+7EG/+UjE58z6g7c0=";
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
