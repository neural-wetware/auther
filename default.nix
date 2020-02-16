let
  pkgs = import (builtins.fetchTarball {
    url = https://releases.nixos.org/nixos/19.09/nixos-19.09.2079.8731aaaf8b3/nixexprs.tar.xz;
    sha256 = "0mwmq6cypiwnl4igqbd0hwd0b0grxn6s2s14qbnap2biwh7vpizl";
  }) {};

  # https://github.com/NixOS/nixpkgs/issues/24647
  my-base32-bytestring = pkgs.haskellPackages.base32-bytestring.overrideAttrs (attr: {
     src = pkgs.fetchFromGitHub {
       owner = "cgag";
       repo = "base32-bytestring";
       rev = "2661ff65d0a1f164bb2e19150079e3913095064b";
       sha256 = "1asd0px79fvzkvpmcklx3000dn18x5skb1bfgkxb8c5nfnyn2rqp";
     };
  });

  srcFilter = path: type: pkgs.lib.cleanSourceFilter path type && baseNameOf path != "default.nix";
in
  with pkgs.haskellPackages;
  mkDerivation {
    pname = "auther";
    version = "1.1";
    src = pkgs.nix-gitignore.gitignoreFilterSource srcFilter []  ./.;
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

    license = pkgs.stdenv.lib.licenses.bsd3;
  }
