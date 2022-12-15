{
  description = "Example Haskell flake showing overrides and adding stuff to the dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    slack-web = {
      url = "github:mercurytechnologies/slack-web/jadel/users-list-pagination";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils, slack-web, ... }:
    let
      ghcVer = "ghc924";
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
            overlays = [ self.overlays.default ];
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
                fourmolu
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

            # someone (me) put too tight lower bounds lol
            hs-opentelemetry-instrumentation-hspec = hlib.doJailbreak hprev.hs-opentelemetry-instrumentation-hspec;
            # buggy output in 4.0.0.0 breaks tests in slack-web
            pretty-simple = hfinal.pretty-simple_4_1_2_0;

            # possible macOS lack-of-sandbox related breakage
            http2 = if prev.stdenv.isDarwin then hlib.dontCheck hprev.http2 else hprev.http2;
            # some kinda weird test issues on macOS
            port-utils = hlib.dontCheck hprev.port-utils;
          });
      };
    };
}
