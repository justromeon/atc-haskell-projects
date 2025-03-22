# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.sqlite
  ];
  nativeBuildInputs = [
    pkgs.haskellPackages.haskell-language-server
  ];

  shellHook = ''
    cabal update
  '';
}
