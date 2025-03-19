{
  description = "Slacklinker Slack link bot";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:mercurytechnologies/flake-compat";
      flake = false;
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks, ... }:
    let
      ghcVer = "ghc98";

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
            config.allowUnfree = true;
          };
          inherit (pkgs) lib;
          hsPkgs = pkgs.haskell.packages.${ghcVer};

        in
        {
          packages = rec {
            default = slacklinker;
            slacklinker = pkgs.haskell.packages.${ghcVer}.slacklinker;
          };

          checks = {
            inherit (self.packages.${system}) slacklinker;

            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              # FIXME(jadel): fourmolu does not understand our language
              # extensions. This is undoubtedly because it doesn't see a cabal
              # file which is in turn undoubtedly because ours is gitignored.
              # Maybe there is a better way to fix this?
              tools.fourmolu = pkgs.writeShellScriptBin "fourmolu" ''
                ${lib.getExe hsPkgs.hpack}
                ${lib.getExe hsPkgs.fourmolu} "$@"
              '';
              hooks = {
                fourmolu.enable = true;
              };
            };
          };

          # for debugging
          inherit pkgs;

          devShells.default =
            hsPkgs.shellFor {
              packages = p: [ self.packages.${system}.slacklinker ];
              withHoogle = true;
              buildInputs = with hsPkgs; [
                haskell-language-server
                fourmolu
                # ghcid
                cabal-install
                graphql-client
                # fast-tags
                # friendly
              ] ++ (with pkgs; [
                ngrok
                sqlite
                refinery-cli
                postgresql
                pgformatter # executable is called pg_format
                cabal2nix
              ]);
              shellHook = self.checks.${system}.pre-commit-check.shellHook;
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${ghcVer} = super.haskell.packages."${ghcVer}".override (oldArgs: {
                overrides =
                  self.lib.fold
                    super.lib.composeExtensions
                    (oldArgs.overrides or (_: _: { }))
                    [ (self.haskell.lib.packageSourceOverrides {
                        slacklinker = ./.;
                      })

                      (self.haskell.lib.packagesFromDirectory {
                        directory = ./nix/deps;
                      })

                      (hself: hsuper: {
                        slacklinker =
                          import ./nix/build.nix
                            { inherit super self hself hsuper;
                              werror = true;
                              testToolDepends = [
                                self.postgresql
                                self.refinery-cli
                              ];
                            }
                            hsuper.slacklinker;

                        # broken bounds >:(
                        hoauth2 =
                          super.haskell.lib.doJailbreak hsuper.hoauth2;

                        tmp-postgres =
                          super.haskell.lib.dontCheck hsuper.tmp-postgres;

                        # possible macOS lack-of-sandbox related breakage
                        http2 =
                          if super.stdenv.isDarwin
                          then super.haskell.lib.dontCheck hsuper.http2
                          else hsuper.http2;

                        # some kinda weird test issues on macOS
                        port-utils =
                          super.haskell.lib.dontCheck hsuper.port-utils;
                      })
                    ];
              });
            };
          };
        };
      };
    };
}
