{
  description = "turnstyle";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskell.packages.ghc98;
      in
      {
        packages = {
          default = haskell.callCabal2nix "turnstyle" ./. { };
        };
        devShells = {
          default = pkgs.mkShell {
            packages = [
              haskell.ghc
              haskell.stylish-haskell
              pkgs.zlib
            ];
          };
        };
      }
    );
}
