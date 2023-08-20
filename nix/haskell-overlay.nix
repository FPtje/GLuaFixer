{ fetchFromGitHub }:
final: previous:
let
  # UUAGC overrides can likely be removed when UUAGC is updated. See
  # https://github.com/UU-ComputerScience/uuagc/pull/12
  uuagc-source = fetchFromGitHub
    {
      owner = "UU-ComputerScience";
      repo = "uuagc";
      rev = "47c09cd1e281e4e224b30bf6e183778e54cdf91c";
      sha256 = "sha256-Ie8oPZtpqW0UikRFBeGsTcEFQ53Z9pPipVC6pv2awYk=";
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
