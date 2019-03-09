let
  pkgs = import <nixpkgs> {};

  # https://github.com/NixOS/nixpkgs/issues/24647
  my-base32-bytestring = pkgs.haskellPackages.base32-bytestring.overrideAttrs (attr: {
     src = pkgs.fetchFromGitHub {
       owner = "cgag";
       repo = "base32-bytestring";
       rev = "2661ff65d0a1f164bb2e19150079e3913095064b";
       sha256 = "1asd0px79fvzkvpmcklx3000dn18x5skb1bfgkxb8c5nfnyn2rqp";
     };
  });

  auther =
    with pkgs.haskellPackages;
    mkDerivation {
      pname = "auther";
      version = "1.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;

      buildTools = [ cabal-install ];

      executableHaskellDepends = [ base ];

      buildDepends = [ hspec byteable ];

      libraryHaskellDepends = [
	my-base32-bytestring
	bytestring
	cryptonite
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
  auther
