(import
  (
    let lock = builtins.fromJSON (builtins.readFile ./flake.lock); in
    fetchTarball {
      url = "https://github.com/mercurytechnologies/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz"
      url = lock.nodes.flake-compat.locked.url;
      sha256 = lock.nodes.flake-compat.locked.narHash;
    }
  )
  { src = ./.; }
).defaultNix
