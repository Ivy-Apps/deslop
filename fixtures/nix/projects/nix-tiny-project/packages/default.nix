{ inputs }:
let
  # The channel, not the flake's pinned nixpkgs.
  pkgs = import <nixpkgs> { };
  dir = ./plugins;
in
{
  hello = import ./hello.nix { inherit pkgs; };
  plugins = map (name: import (./plugins + "/${name}.nix")) [ "a" "b" ];
  extra = import (dir + "/extra.nix");
  gone = import ./gone.nix;
}
