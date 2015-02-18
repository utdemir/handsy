{ nixpkgs ? { outPath = ../nixpkgs; revCount = 1234; gitTag = "dirty"; } 
, system ? "x86_64-linux"
}:

let
    pkgs = (import nixpkgs { inherit system; });
    hs = pkgs.haskellngPackages;
in

hs.mkDerivation {
  pname = "handsy";
  version = "0.0.10";
  license = "unknown";
  isLibrary = true;
  src = ./.;
  
  buildDepends = [
    hs.base hs.cabal-install hs.free hs.process-extras hs.shell-escape
    hs.tasty hs.tasty-hunit hs.tasty-th hs.retry hs.data-default-class
    hs.split
  ];
}
