{
  description = "Example Haskell flake showing overrides and adding stuff to the dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    slack-web = {
      url = "github:mercurytechnologies/slack-web";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # https://github.com/mpickering/apply-refact/pull/128
    apply-refact = {
      url = "github:july541/apply-refact/ghc-9.4";
      flake = false;
    };

  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils, slack-web, apply-refact, ... }:
    let
      ghcVer = "ghc943";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev final);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default self.overlays.hls ];
            config.allowBroken = true;
            config.allowUnfree = true;
          };

        in
        {
          packages = rec {
            default = slacklinker;
            slacklinker = pkgs.haskell.packages.${ghcVer}.slacklinker;
          };

          checks = {
            inherit (self.packages.${system}) slacklinker;
          };

          # for debugging
          inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: [ self.packages.${system}.slacklinker ];
              withHoogle = true;
              buildInputs = with haskellPackages; [
                haskell-language-server
                # fourmolu
                # ghcid
                cabal-install
                # fast-tags
                # friendly
              ] ++ (with pkgs; [
                ngrok
                sqlite
                refinery-cli
                postgresql
                pgformatter # executable is called pg_format
              ]);
              # Change the prompt to show that you are in a devShell
              # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        hls = import ./nix/hls.nix { inherit ghcVer; };
        default = makeHaskellOverlay (prev: final: hfinal: hprev:
          let
            hlib = prev.haskell.lib;
            build = import ./nix/build.nix {
              inherit prev final hfinal hprev;
              werror = true;
              testToolDepends = [
                final.postgresql
                final.refinery-cli
              ];
            };
            slacklinker = hprev.callCabal2nix "slacklinker" ./. { };
          in
          {
            slacklinker = build slacklinker;
            slack-web = hprev.callCabal2nix "slack-web" slack-web { };

            # Older version does not build on ghc 9.4 (this should be in nixpkgs!)
            hlint = hfinal.hlint_3_5;

            # hangs in test on my machine
            ListLike = hlib.dontCheck hprev.ListLike;

            # someone (me) put too tight lower bounds lol
            hs-opentelemetry-instrumentation-hspec = hlib.doJailbreak hprev.hs-opentelemetry-instrumentation-hspec;

            # possible macOS lack-of-sandbox related breakage
            http2 = if prev.stdenv.isDarwin then hlib.dontCheck hprev.http2 else hprev.http2;
            # some kinda weird test issues on macOS
            port-utils = hlib.dontCheck hprev.port-utils;

            persistent = hfinal.persistent_2_14_3_0;
            refined = hfinal.refined_0_8;

            # bounds on hspec:
            # https://github.com/haskell-servant/servant/pull/1629
            servant = hlib.doJailbreak hprev.servant;
            servant-server = hlib.doJailbreak hprev.servant-server;
            servant-client-core = hlib.doJailbreak hprev.servant-client-core;
            servant-client = hlib.doJailbreak hprev.servant-client;

            # tasty-hedgehog is broken, these use it in their testsuite
            hedgehog-fn = hlib.doJailbreak hprev.hedgehog-fn;
            nonempty-containers = hlib.dontCheck hprev.nonempty-containers;
            retry = hlib.dontCheck hprev.retry;
            uri-bytestring = hlib.dontCheck hprev.uri-bytestring;

            # Newer version
            ghc-lib-parser = hfinal.callHackage "ghc-lib-parser" "9.4.2.20220822" { };
            ghc-lib-parser-ex = hfinal.callHackage "ghc-lib-parser-ex" "9.4.0.0" { };

            # seems to hang in test on my machine :(
            ghc-exactprint = hlib.dontCheck (hfinal.callPackage ./nix/deps/ghc-exactprint.nix { });
            retrie = hlib.dontCheck (hlib.doJailbreak (hfinal.callPackage ./nix/deps/retrie.nix { }));
            apply-refact = hprev.callCabal2nix "apply-refact" apply-refact { };

            # Newer version
            fourmolu =
              hlib.overrideCabal
                (hlib.disableCabalFlag hprev.fourmolu_0_8_2_0 "fixity-th")
                (old: {
                  # Update to Fourmolu 0.9 for GHC 9.4 support.
                  version = "0.9.0.0";
                  src = final.fetchFromGitHub {
                    owner = "fourmolu";
                    repo = "fourmolu";
                    # Branch: `main`
                    rev = "47017e0f7c333676f3fd588695c1b3d16f2075cc";
                    hash = "sha256-MPFWDMc9nSpTtCjAIHmjLyENkektm72tCWaKnkAQfuk=";
                  };
                  libraryHaskellDepends = (old.libraryHaskellDepends or [ ]) ++ [ hfinal.file-embed ];
                })
            ;

          });
      };
    };
}
