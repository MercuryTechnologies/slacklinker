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
      ghcVer = "ghc910";

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (final: prev: { hsPkgs = pkgs.haskell.packages.${ghcVer}; })
              self.overlays.packages-dir
              self.overlays.default
            ];
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

          legacyPackages = {
            inherit pkgs;
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
                ${lib.getExe pkgs.mercury.run-fourmolu} "$@"
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
                treefmt
                postgresql
                pgformatter # executable is called pg_format
                cabal2nix
                process-compose
                mercury.run-fourmolu
              ]);
              shellHook = self.checks.${system}.pre-commit-check.shellHook;
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        packages-dir = final: prev: {
          mercury = (prev.lib.packagesFromDirectoryRecursive {
            inherit (prev) callPackage newScope;
            directory = ./nix/packages;
          });
        };
        default = self: super:
        let inherit (self.haskell.lib) dontCheck doJailbreak packagesFromDirectory packageSourceOverrides;
        in {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${ghcVer} = super.haskell.packages."${ghcVer}".override (oldArgs: {
                overrides =
                  self.lib.foldr
                    super.lib.composeExtensions
                    (oldArgs.overrides or (_: _: { }))
                    [ (packageSourceOverrides {
                        slacklinker = ./.;
                      })

                      (packagesFromDirectory {
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

                        tmp-postgres = dontCheck hsuper.tmp-postgres;

                        # bounds...
                        hs-opentelemetry-instrumentation-wai = doJailbreak hsuper.hs-opentelemetry-instrumentation-wai;
                        hs-opentelemetry-instrumentation-hspec = doJailbreak hsuper.hs-opentelemetry-instrumentation-hspec;
                        hs-opentelemetry-instrumentation-conduit = doJailbreak hsuper.hs-opentelemetry-instrumentation-conduit;
                        hs-opentelemetry-instrumentation-http-client = doJailbreak hsuper.hs-opentelemetry-instrumentation-http-client;
                        hs-opentelemetry-instrumentation-persistent = doJailbreak hsuper.hs-opentelemetry-instrumentation-persistent;
                      })
                    ];
              });
            };
          };
        };
      };
    };
}
