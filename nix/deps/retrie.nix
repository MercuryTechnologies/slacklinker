{
  mkDerivation,
  ansi-terminal,
  async,
  base,
  bytestring,
  containers,
  data-default,
  deepseq,
  directory,
  exceptions,
  fetchgit,
  filepath,
  ghc,
  ghc-exactprint,
  ghc-paths,
  haskell-src-exts,
  HUnit,
  lib,
  list-t,
  mtl,
  optparse-applicative,
  process,
  random-shuffle,
  syb,
  tasty,
  tasty-hunit,
  temporary,
  text,
  transformers,
  unordered-containers,
}:
mkDerivation {
  pname = "retrie";
  version = "1.2.0.1";
  src = fetchgit {
    url = "https://github.com/MercuryTechnologies/retrie";
    sha256 = "1df69827wn20l6kqvrdr9kcv464xb2890jyphxviz8k6ifvkqll4";
    rev = "1e21960e8007de514244b1935e1f5e553af49c2d";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal
    async
    base
    bytestring
    containers
    data-default
    directory
    filepath
    ghc
    ghc-exactprint
    list-t
    mtl
    optparse-applicative
    process
    random-shuffle
    syb
    text
    transformers
    unordered-containers
  ];
  executableHaskellDepends = [base ghc-paths haskell-src-exts];
  testHaskellDepends = [
    base
    containers
    data-default
    deepseq
    directory
    exceptions
    filepath
    ghc
    ghc-exactprint
    ghc-paths
    haskell-src-exts
    HUnit
    mtl
    optparse-applicative
    process
    syb
    tasty
    tasty-hunit
    temporary
    text
    unordered-containers
  ];
  homepage = "https://github.com/facebookincubator/retrie";
  description = "A powerful, easy-to-use codemodding tool for Haskell";
  license = lib.licenses.mit;
}
