{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.python3
    pkgs.python3Packages.pillow
  ];
}
