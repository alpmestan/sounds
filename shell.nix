{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let hs = haskellPackages;
in
mkShell {
  name = "sound";
  buildInputs = [
    ghc
    cabal-install
    hs.haskell-language-server
    libpulseaudio
    pkg-config
    zlib
  ];
}
