{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.reflex-dom
    pkgs.haskellPackages.reflex-dom-ajax
    pkgs.nodejs
  ];
  shellHook = ''
    echo "Reflex frontend dev shell ready. Use nix-build or ghcjs to compile when configured."
  '';
}
