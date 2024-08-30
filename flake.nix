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

                        # broken bounds. as mercury people, you can fix this
                        # upstream :)
                        slack-web =
                          super.haskell.lib.doJailbreak hsuper.slack-web;

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
