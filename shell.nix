let
  pkgs = import <nixpkgs> {};
  transit-haskell = pkgs.haskellPackages.callCabal2nix "transit-haskell" ./. {};
in
  pkgs.mkShell {
    inputsFrom = [
      transit-haskell.env
    ];
    buildInputs = [
      pkgs.ghcid
      pkgs.cabal-install
      pkgs.haskellPackages.ghc
      pkgs.haskellPackages.hoogle
      pkgs.haskellPackages.hlint
    ];
  }
