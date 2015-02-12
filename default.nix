{ nixpkgsSrc ? { outPath = ../nixpkgs; revCount = 1234; gitTag = "dirty"; } 
, system ? "x86_64-linux"
}:

let
    pkgs = (import nixpkgsSrc { inherit system; });
    hs = pkgs.haskellngPackages;
in

hs.mkDerivation {
  pname = "handsy";
  version = "0.0.9";
  license = "unknown";
  isLibrary = true;
  src = ./.;
  
  buildDepends = [
    hs.base hs.cabal-install hs.free hs.process-extras hs.shell-escape
    hs.tasty hs.tasty-hunit hs.retry hs.implicit-params hs.data-default-class
  ];
}
