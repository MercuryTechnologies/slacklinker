{ writeShellApplication, hsPkgs, yq-go }:
writeShellApplication {
  name = "run-fourmolu";
  runtimeInputs = [
    hsPkgs.fourmolu
    yq-go
  ];
  text = builtins.readFile ./run-fourmolu.sh;
}
