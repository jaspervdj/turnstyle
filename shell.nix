{ pkgs ? import <nixpkgs> { }, fish ? true }:
pkgs.mkShell {
    inputsFrom = [ (import ./default.nix).env ];
    packages = [
        pkgs.cabal-install
        pkgs.entr
        pkgs.git
        pkgs.stylish-haskell
    ] ++ (if fish then [pkgs.fish] else []);
    shellHook = if fish then "exec fish" else null;
}
