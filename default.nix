{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:
 pkgs.haskell.packages.${compiler}.callPackage ./bittrex.nix {}
