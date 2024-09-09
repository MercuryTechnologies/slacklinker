{ mkDerivation, async, attoparsec, base, binary, bytestring
, charset, clock, containers, ghc-prim, hashable, hspec, http-types
, lib, memory, mtl, safe-exceptions, template-haskell, text
, thread-utils-context, transformers, unliftio-core
, unordered-containers, vault, vector, vector-builder
}:
mkDerivation {
  pname = "hs-opentelemetry-api";
  version = "0.1.0.0";
  sha256 = "1bi0qzlwn5k9x5j9lvv97m85ckmpvywigy3jajw2rxi8zi84v9s2";
  postUnpack = "sourceRoot+=/api/hs-opentelemetry-api.cabal; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    async attoparsec base binary bytestring charset clock containers
    ghc-prim hashable http-types memory mtl safe-exceptions
    template-haskell text thread-utils-context transformers
    unliftio-core unordered-containers vault vector vector-builder
  ];
  testHaskellDepends = [
    async attoparsec base binary bytestring charset clock containers
    ghc-prim hashable hspec http-types memory mtl safe-exceptions
    template-haskell text thread-utils-context transformers
    unliftio-core unordered-containers vault vector vector-builder
  ];
  homepage = "https://github.com/iand675/hs-opentelemetry#readme";
  description = "OpenTelemetry API for use by libraries for direct instrumentation or wrapper packages";
  license = lib.licenses.bsd3;
}
