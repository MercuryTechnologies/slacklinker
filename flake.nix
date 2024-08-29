{
  description = "Slacklinker Slack link bot";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:mercurytechnologies/flake-compat";
      flake = false;
    };
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      ghcVer = "ghc98";
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

            # broken bounds. as mercury people, you can fix this upstream :)
            slack-web = hlib.doJailbreak hprev.slack-web;

            tmp-postgres = hlib.overrideSrc hprev.tmp-postgres ({
              src = final.fetchFromGitHub {
                owner = "lambdamechanic";
                repo = "tmp-postgres";
                # https://github.com/lambdamechanic/tmp-postgres/tree/master
                rev = "4c4f4346ea5643d09cee349edac9060fab95a3cb";
                sha256 = "sha256-vzfJIrzW7rRpA18rEAHVgQdKuEQ5Aep742MkDShxtj0=";
              };
            });

            # possible macOS lack-of-sandbox related breakage
            http2 = if prev.stdenv.isDarwin then hlib.dontCheck hprev.http2 else hprev.http2;
            # some kinda weird test issues on macOS
            port-utils = hlib.dontCheck hprev.port-utils;
          });
      };
    };
}
