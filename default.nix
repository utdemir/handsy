{ nixpkgsSrc ? { outPath = ../nixpkgs; revCount = 1234; gitTag = "dirty"; } 
, system ? "x86_64-linux"
}:

let
    pkgs = (import nixpkgsSrc { inherit system; });
    hs = pkgs.haskellngPackages;
in

hs.mkDerivation {
  pname = "handsy";
  version = "0.1";
  license = "unknown";
  isLibrary = true;
  src = ./.;
  
  buildDepends = [
    hs.base hs.cabal-install hs.free hs.process-extras
  ];
}
