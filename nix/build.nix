{ prev, hfinal, hprev, werror }: slacklinker:
let
  hlib = prev.haskell.lib;
  lib = prev.lib;
in
(hlib.overrideCabal slacklinker
  (drv: {
    configureFlags = (drv.configureFlags or [ ])
      ++ lib.optionals werror [ "--ghc-option=-Werror" ];
    # primarily motivated by the fact that we have a test-dev testsuite that we
    # would not like to build pointlessly
    checkPhase = ''
      runHook preCheck
      ./Setup test --show-details=direct $checkFlags ''${checkFlagsArray:+"''${checkFlagsArray[@]}"}
      runHook postCheck
    '';
  })).overrideAttrs (old: {
  # Don’t allow focused tests to pass CI
  HSPEC_OPTIONS = "--fail-on-focused";

})
