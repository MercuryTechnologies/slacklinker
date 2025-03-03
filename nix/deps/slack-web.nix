{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, bytestring, classy-prelude, containers, crypton
, data-default-class, deepseq, either, errors, fakepull
, generic-arbitrary, hashable, hspec, hspec-core, hspec-discover
, hspec-golden, http-api-data, http-client, http-client-tls, lib
, megaparsec, mono-traversable, mtl, pretty-simple, QuickCheck
, quickcheck-instances, refined, scientific, servant
, servant-client, servant-client-core, string-conversions
, string-variants, template-haskell, text, th-compat, time
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "slack-web";
  version = "2.1.0.0";
  sha256 = "8606da0f7b0e0c373eb219a3fe4bc3cc82e61a1e8999df55f041ae41d6a036d0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring classy-prelude containers
    crypton data-default-class deepseq either errors hashable
    http-api-data http-client http-client-tls megaparsec
    mono-traversable mtl refined scientific servant servant-client
    servant-client-core string-conversions string-variants text time
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring classy-prelude fakepull
    generic-arbitrary hspec hspec-core hspec-golden mtl pretty-simple
    QuickCheck quickcheck-instances refined string-conversions
    string-variants template-haskell text th-compat time
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/MercuryTechnologies/slack-web";
  description = "Bindings for the Slack web API";
  license = lib.licenses.mit;
}
