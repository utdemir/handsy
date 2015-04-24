{ mkDerivation, base, bytestring, data-default-class, errors
, lifted-base, operational, process-extras, retry, shell-escape
, split, stdenv, tasty, tasty-hunit, tasty-th, transformers
}:
mkDerivation {
  pname = "handsy";
  version = "0.0.13";
  src = ./.;
  buildDepends = [
    base bytestring data-default-class errors lifted-base operational
    process-extras retry shell-escape split transformers
  ];
  testDepends = [ base bytestring tasty tasty-hunit tasty-th ];
  homepage = "https://github.com/utdemir/handsy";
  description = "A DSL to describe common shell operations and interpeters for running them locally and remotely";
  license = stdenv.lib.licenses.bsd3;
}
