{ pkgs ? import <nixpkgs> {} }:
 pkgs.haskellPackages.callPackage ./bittrex.nix {}
