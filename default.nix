{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "monad-network-instances" ./. {}
