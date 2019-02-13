let
  pkgs = import <nixpkgs> {};

  auther = { mkDerivation, base, stdenv }:
    with pkgs.haskellPackages;
    mkDerivation {
      pname = "auther";
      version = "1.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [ base ];
      buildDepends = [ hspec byteable ];
      libraryHaskellDepends = [
	      bytestring
	      cryptonite
	      dataenc
	      binary
	      memory
	      clock
	      array
	      vector
	      unix
	      word8
	      bytestring-conversion
      ];
      license = stdenv.lib.licenses.bsd3;
    };
in
  pkgs.haskellPackages.callPackage auther {}
