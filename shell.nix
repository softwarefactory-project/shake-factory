let
  pkgs = import <nixpkgs> {};
  sf = import ./default.nix;
in pkgs.mkShell {
    buildInputs = sf;
  }
