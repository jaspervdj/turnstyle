{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell rec {
  buildInputs = [
    cabal-install
    haskell.compiler.ghc963
    nodejs
    pkg-config
    zlib
  ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);

  # Ensure that libz.so and other libraries are available to TH
  # splices, cabal repl, etc.
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
