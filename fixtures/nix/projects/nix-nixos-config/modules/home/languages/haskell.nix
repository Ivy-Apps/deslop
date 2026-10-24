{ pkgs, ... }:
let
  hpkgs = pkgs.haskell.packages.ghc9103;
in
{
  home.packages = [
    pkgs.cabal-install
    pkgs.hlint
    pkgs.fourmolu
    hpkgs.haskell-language-server
    hpkgs.implicit-hie
    hpkgs.cabal-fmt
    (pkgs.haskell.lib.compose.justStaticExecutables hpkgs.hspec-golden)
  ];
}
