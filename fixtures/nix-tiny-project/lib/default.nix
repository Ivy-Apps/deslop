{ lib }:
let
  helpers = import ./helpers.nix;
  # Computed path: only evaluation knows which files this reaches.
  plugins = map (name: import (./. + "/plugins/${name}.nix")) [ "x" "y" ];
in
{
  inherit helpers plugins;
}
