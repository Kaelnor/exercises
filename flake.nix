{
  description = "Exercises for the Haskell Beginners 2022 course";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        darwinBuildInputs = with pkgs; lib.optionals stdenv.isDarwin [
          darwin.apple_sdk.frameworks.CoreServices
          libiconv
        ];
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.ghc92
            cabal-install
            (haskell-language-server.override { supportedGhcVersions = [ "92" ]; })
            hlint
            ormolu
          ] ++ darwinBuildInputs;
        };
      });
}
