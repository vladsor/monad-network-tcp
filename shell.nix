{ pkgs ? import <nixpkgs> {} }:

let
  drv = (import ./. { inherit pkgs; });
in
  (if pkgs.lib.inNixShell then drv.env else drv)
    .overrideAttrs(oldAttrs: { buildInputs = oldAttrs.buildInputs ++ [ pkgs.haskellPackages.cabal-install ]; })
