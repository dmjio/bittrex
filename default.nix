{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
 pkgs.haskell.packages.${compiler}.callPackage ./bittrex.nix {}
