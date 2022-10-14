{
  description = "Example Haskell flake showing overrides and adding stuff to the dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    slack-web = {
      url = "github:mercurytechnologies/slack-web";
      flake = false;
    };
    mono-traversable = {
      url = "github:snoyberg/mono-traversable";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils, slack-web, mono-traversable, ... }:
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
            };
            slacklinker = hprev.callCabal2nix "slacklinker" ./. { };
          in
          {
            slacklinker = build slacklinker;
            slack-web = hprev.callCabal2nix "slack-web" slack-web { };
            string-variants = hprev.callHackageDirect { pkg = "string-variants"; ver = "0.1.0.1"; sha256 = "sha256-7oNYwPP8xRNYxKNdNH+21zBHdeUeiWBtKOK5G43xtSQ="; } { };

            # jackage in the testsuite
            mutable-containers = hprev.callCabal2nixWithOptions "mutable-containers" mono-traversable "--subpath=mutable-containers" { };
          });
      };
    };
}
