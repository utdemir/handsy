{ nixpkgsSrc ? { outPath = ../nixpkgs; revCount = 1234; gitTag = "dirty"; } 
, system ? "x86_64-linux"
, devel ? true
}:

let
    pkgs = (import nixpkgsSrc { inherit system; });
    hs = pkgs.haskellPackages;
    inherit (hs) cabal;
in

cabal.mkDerivation (self: {
  pname = "handsy";
  version = "0.1";
  isLibrary = true;
  src = if devel then ./. else pkgs.fetchgit { url = ./.; };

  buildDepends = [
    hs.cabalInstall hs.free pkgs.openssh
  ];
})
