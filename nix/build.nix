{ super, self, hself, hsuper, werror, testToolDepends ? [] }: slacklinker:
let
  hlib = super.haskell.lib;
  lib = super.lib;
  extraFiles = [ ../config ../db ];
  copy = f: "cp -R ${f} $out/${builtins.baseNameOf f}";
  migrateDeps = [
    self.refinery-cli
  ];

  # determined through use of `strings | grep /nix/store`
  # allegedly enableSeparateDataOutput does something, but I've not found that
  # to be true. So just nuke it. w/e.
  badReferences = [
    hself.hs-opentelemetry-sdk
    hself.warp
  ];

  referencesCmdline = lib.concatMapStrings (x: "-t ${x} ") badReferences;
in
(hlib.overrideCabal (hlib.justStaticExecutables slacklinker)
  (drv: {
    configureFlags = (drv.configureFlags or [ ])
      ++ lib.optionals werror [ "--ghc-option=-Werror" ];

    testToolDepends = (drv.testToolDepends or [ ]) ++ testToolDepends;

    # chmod: hack since the extraFiles are in the nix store and cp preserves
    # permissions (desirable except for the lack of write bit)
    #
    # remove-references-to: heinous hack to deal with bonus stuff ending up in
    # closure:
    # https://github.com/NixOS/nixpkgs/issues/42095
    postInstall = ''
      ${lib.concatStringsSep "\n" (map copy extraFiles)}
      chmod -R u+w $out
      wrapProgram "$out/db/migrate.sh" --prefix PATH : "${lib.makeBinPath migrateDeps}"

      remove-references-to ${referencesCmdline} $out/bin/*
    '';
  })).overrideAttrs (old: {
  # Don’t allow focused tests to pass CI
  HSPEC_OPTIONS = "--fail-on=focused";

  disallowedReferences = badReferences;

  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
    self.makeWrapper
  ];
})
