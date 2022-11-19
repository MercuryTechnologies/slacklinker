# vendored from the backend; much of HLS is unreleased as yet
{ ghcVer }: self: super:
let
  inherit
    (self.haskell.lib.compose)
    dontCheck
    doJailbreak
    overrideCabal
    allowInconsistentDependencies
    ;

  src = self.fetchFromGitHub {
    owner = "MercuryTechnologies";
    repo = "haskell-language-server";
    # Branch: `rebeccat/ghc94`
    rev = "1b027bf8b053ef6e0015f69b6bb45b40c52e4693";
    hash = "sha256-niO7c9uKEWKbDvQAFjpT7U4VszuFS0pdfcNMN7l9bxk=";
  };
in
{
  haskell =
    super.haskell
    // {
      packages =
        super.haskell.packages
        // {
          ${ghcVer} = super.haskell.packages.${ghcVer}.override (old: {
            overrides =
              self.lib.composeExtensions (old.overrides or (_: _: { }))
                (hself: hsuper:
                  let
                    # The test suite for the HLS plugins fail weirdly in Nix.
                    # Reported upstream here: https://github.com/haskell/haskell-language-server/issues/2312
                    # There seem to be two classes of errors.
                    # One is caused by attempting to write to `$HOME`, which is
                    # set to the nonexistent path `/homeless-shelter` (ugh) in
                    # Nix builds:
                    #     Error while initializing: ResponseError
                    #       { _code = InternalError
                    #       , _message = "Error on initialize: /homeless-shelter: createDirectory: permission denied (Read-only file system)"
                    #       , _xdata = Nothing}
                    # The other class of error is from... something getting messed up:
                    #     IO exception:
                    #     <file descriptor: 116>: hPutBuf: resource vanished (Broken pipe)
                    #     arose while trying to send message:
                    #     {
                    #         "id": 0,
                    #         "jsonrpc": "2.0",
                    #         "method": "shutdown",
                    #         "params": null
                    #     }
                    hlsPlugin = name: dontCheck (hself.callCabal2nixWithOptions name src "--subpath plugins/${name}" { });

                    hlsPackage = name: hself.callCabal2nixWithOptions name src "--subpath ${name}" { };
                  in
                  {
                    # These are all the HLS plugins except for non-Fourmolu formatters:
                    hls-refactor-plugin = hlsPlugin "hls-refactor-plugin";
                    hls-splice-plugin = hlsPlugin "hls-splice-plugin";
                    hls-hlint-plugin = hlsPlugin "hls-hlint-plugin";
                    hls-class-plugin = hlsPlugin "hls-class-plugin";
                    hls-explicit-fixity-plugin = hlsPlugin "hls-explicit-fixity-plugin";
                    hls-alternate-number-format-plugin = hlsPlugin "hls-alternate-number-format-plugin";
                    hls-explicit-record-fields-plugin = hlsPlugin "hls-explicit-record-fields-plugin";
                    hls-call-hierarchy-plugin = hlsPlugin "hls-call-hierarchy-plugin";
                    hls-haddock-comments-plugin = hlsPlugin "hls-haddock-comments-plugin";
                    hls-eval-plugin = hlsPlugin "hls-eval-plugin";
                    hls-explicit-imports-plugin = hlsPlugin "hls-explicit-imports-plugin";
                    hls-refine-imports-plugin = hlsPlugin "hls-refine-imports-plugin";
                    hls-rename-plugin = hlsPlugin "hls-rename-plugin";
                    hls-retrie-plugin = hlsPlugin "hls-retrie-plugin";
                    hls-tactics-plugin = hlsPlugin "hls-tactics-plugin";
                    hls-stan-plugin = hlsPlugin "hls-stan-plugin";
                    hls-module-name-plugin = hlsPlugin "hls-module-name-plugin";
                    hls-qualify-imported-names-plugin = hlsPlugin "hls-qualify-imported-names-plugin";
                    hls-code-range-plugin = hlsPlugin "hls-code-range-plugin";
                    hls-change-type-signature-plugin = hlsPlugin "hls-change-type-signature-plugin";
                    hls-gadt-plugin = hlsPlugin "hls-gadt-plugin";

                    # Fixes this bug:
                    # This package indirectly depends on multiple versions of the same package. This is very likely to cause a compile failure.
                    #   package fourmolu (fourmolu-0.8.2.0-4NdtEWIsUDkB6NJ3E6EpRj) requires Cabal-syntax-3.8.1.0
                    #   package implicit-hie (implicit-hie-0.1.3.0-1UUxSgzLz4u6wYfo3lMeJ9) requires Cabal-syntax-3.8.1.0-2SaXfdvRXcVccvq7QmNcZ
                    hls-fourmolu-plugin = allowInconsistentDependencies (doJailbreak (hlsPlugin "hls-fourmolu-plugin"));

                    hls-plugin-api = hlsPackage "hls-plugin-api";
                    hls-graph = hlsPackage "hls-graph";
                    hls-test-utils = hlsPackage "hls-test-utils";

                    # Test failures
                    ghcide = dontCheck (hlsPackage "ghcide");

                    # Needed for `hls-explicit-record-fields-plugin`.
                    # New version needed to compile on GHC 9.4.
                    hw-prim = hself.callHackage "hw-prim" "0.6.3.2" { };

                    haskell-language-server =
                      overrideCabal
                        (old: {
                          # Nixpkgs patches the `haskell-language-server` startup
                          # script to expect the ABI hashes to match the output of
                          # `ghc-pkg --global list --simple-output`.  However,
                          # because we override the default value of Cabal there is
                          # still a slight mismatch in the ABI (for the `Cabal`
                          # package).
                          #
                          # This mismatch is benign, though, so we turn off the ABI
                          # check.  Indeed, you can skip the ABI check entirely by
                          # using `haskell-language-server-wrapper` instead (which
                          # works fine), but we want LSP clients to pick up the
                          # versions without the `-wrapper` suffix.
                          #
                          # The way we turn off the ABI check here is to delete
                          # the `return 3` command which is what would normally exit
                          # if the ABI check failed.
                          postInstall =
                            (old.postInstall or "")
                            + ''
                              ${self.buildPackages.gnused}/bin/sed -i 's/return 3//' $out/bin/haskell-language-server
                            '';
                        })
                        # Roughly the same inconsistent dependencies as `hls-fourmolu-plugin` above.
                        # The test suite expects to find Stack and GHC 8.10 and other
                        # silly tools, so we disable it.
                        (dontCheck (allowInconsistentDependencies ((hself.callCabal2nix "haskell-language-server" src { }).override {
                          # The reason we do this is to guarantee that
                          # `haskell-language-server` supports at least the following
                          # plugins when we upgrade GHC, even if Nixpkgs tries to
                          # disable those plugins, such as here:
                          #
                          # https://github.com/NixOS/nixpkgs/blob/a1826e78afb277ba1c5fbb54e796371eb664091e/pkgs/development/haskell-modules/configuration-ghc-9.4.x.nix#L209-L226
                          #
                          # Note that this doesn't account for `buildable: False`
                          # being set in the cabal files of any of the plugins, or
                          # plugins only being included in an `impl(ghc)` block in
                          # HLS's cabal file.
                          inherit
                            (hself)
                            hls-call-hierarchy-plugin
                            hls-code-range-plugin
                            hls-explicit-imports-plugin
                            hls-fourmolu-plugin
                            hls-hlint-plugin
                            hls-pragmas-plugin
                            hls-refactor-plugin
                            ;
                        })));
                  });
          });
        };
    };

  haskell-language-server = super.haskell-language-server.override {
    haskellPackages = self.haskell.packages.${ghcVer};

    supportedGhcVersions =
      let
        number =
          builtins.replaceStrings [ "ghc" ] [ "" ] ghcVer;
      in
      [ number ];
  };
}
