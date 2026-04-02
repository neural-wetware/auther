{
  description = "Haskell auther app using custom base32-bytestring, fully clean";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghc = pkgs.haskell.packages.ghc96;

        auther = ghc.callCabal2nix "auther" ./. {};
      in {
        packages.default = auther;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghc.ghc
            ghc.cabal-install
          ];
        };
      });
}
