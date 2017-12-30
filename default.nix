{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
 pkgs.haskell.packages.${compiler}.callPackage ./bittrex.nix {}
