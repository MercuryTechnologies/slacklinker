{ system ? builtins.currentSystem
, ...
}:
let
  defaultNix = import ./default.nix;
in
{
  build = defaultNix.packages.${system}.default;
}
