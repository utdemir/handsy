with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, data-default-class, errors
             , lifted-base, operational, process-extras, retry, shell-escape
             , split, stdenv, tasty, tasty-hunit, tasty-th, transformers
             , cabal-install
             }:
             mkDerivation {
               pname = "handsy";
               version = "0.0.13";
               src = ./.;
               buildDepends = [
                 base bytestring data-default-class errors lifted-base operational
                 process-extras retry shell-escape split transformers cabal-install
               ];
               testDepends = [ base bytestring tasty tasty-hunit tasty-th ];
               homepage = "https://github.com/utdemir/handsy";
               description = "A DSL to describe common shell operations and interpeters for running them locally and remotely";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
