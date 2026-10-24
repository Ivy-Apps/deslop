{ pkgs, ... }:
{
  home.packages = with pkgs; [
    d2 # CLI tool; also provides `d2 fmt` used by conform-nvim
  ];
}
