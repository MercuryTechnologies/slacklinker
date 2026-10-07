{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, bytestring, classy-prelude, containers, crypton, data-default
, deepseq, either, errors, fakepull, fetchgit, generic-arbitrary
, hashable, hspec, hspec-core, hspec-discover, hspec-golden
, http-api-data, http-client, http-client-tls, lib, megaparsec
, mono-traversable, mtl, pretty-simple, QuickCheck
, quickcheck-instances, refined, scientific, servant
, servant-client, servant-client-core, string-conversions
, string-variants, template-haskell, text, th-compat, time
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "slack-web";
  version = "2.3.0.0";
  src = fetchgit {
    url = "https://github.com/MercuryTechnologies/slack-web";
    sha256 = "0k6xa5g3lw9hw9my54j21ig15sws9mlm2vaxsdgj908zfr8pycs4";
    rev = "1ea1e48f75e8ee418ec18338af8a1c271415a50c";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring classy-prelude containers
    crypton data-default deepseq either errors hashable http-api-data
    http-client http-client-tls megaparsec mono-traversable mtl refined
    scientific servant servant-client servant-client-core
    string-conversions string-variants text time transformers
    unordered-containers vector
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
  license = lib.licensesSpdx."MIT";
}
