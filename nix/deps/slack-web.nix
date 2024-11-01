{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, bytestring, classy-prelude, containers, crypton
, data-default-class, deepseq, either, errors, fakepull, fetchgit
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
  version = "2.0.0.4";
  src = fetchgit {
    url = "https://github.com/MercuryTechnologies/slack-web";
    sha256 = "sha256-sMmlGMzbj8xy+joqN/Ohok1Q+B3pZDUFCMnqu5xi+a8=";
    rev = "649186a790a79547ecd16836af5789c93a195a95";
    fetchSubmodules = true;
  };
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
