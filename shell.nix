{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
    inputsFrom = [ (import ./default.nix).env ];
    packages = [
        pkgs.cabal-install
    ];
}
