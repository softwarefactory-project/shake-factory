# A list of derivation for shake and the shake-factory library.
let
  pkgs = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/a6f258f49fcd1644f08b7b3677da2c5e55713291.tar.gz")
    { };
  hsPkgs = pkgs.haskell.packages.ghc8107.override {
    overrides = hpFinal: hpPrev: {
      shake-dhall = pkgs.haskell.lib.dontCheck
        (pkgs.haskell.lib.overrideCabal hpPrev.shake-dhall { broken = false; });

    };
  };

  shake-factory = hsPkgs.callCabal2nix "shake-factory" ./. { };

  ghc = hsPkgs.ghcWithPackages (p: [ shake-factory ]);
in [ hsPkgs.shake ghc ]
