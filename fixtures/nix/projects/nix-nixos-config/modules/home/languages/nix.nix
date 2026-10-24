{ pkgs, ... }:
{
  home.packages = [
    pkgs.nil
    pkgs.nixfmt
    pkgs.statix
    pkgs.deadnix
    pkgs.nix-tree
  ];
}
