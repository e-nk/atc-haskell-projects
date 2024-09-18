# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
  ];

  shellHook = ''
    cabal update
  '';
}
