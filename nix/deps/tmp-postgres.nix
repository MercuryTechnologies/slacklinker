{ mkDerivation, async, base, base64-bytestring, bytestring
, containers, criterion, cryptohash-sha1, deepseq, directory
, fetchgit, generic-monoid, hspec, lib, mtl, network, port-utils
, postgres-options, postgresql-simple, prettyprinter, process, stm
, temporary, transformers, unix
}:
mkDerivation {
  pname = "tmp-postgres";
  version = "1.35.0.0";
  src = fetchgit {
    url = "https://github.com/lambdamechanic/tmp-postgres.git";
    sha256 = "0gdnf4l0s933wdxyl09r8jw4l1w1sl0i0asz0dlv9vnnphicjdxz";
    rev = "4c4f4346ea5643d09cee349edac9060fab95a3cb";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base base64-bytestring bytestring containers cryptohash-sha1
    deepseq directory generic-monoid port-utils postgres-options
    postgresql-simple prettyprinter process stm temporary transformers
    unix
  ];
  executableHaskellDepends = [
    async base directory postgres-options postgresql-simple process
    temporary
  ];
  testHaskellDepends = [
    async base containers directory generic-monoid hspec mtl network
    port-utils postgres-options postgresql-simple process temporary
    unix
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq postgres-options postgresql-simple temporary
  ];
  homepage = "https://github.com/jfischoff/tmp-postgres#readme";
  description = "Start and stop a temporary postgres";
  license = lib.licenses.bsd3;
}
