{
  root,
  pkgs,
  inputs,
  lib ? pkgs.lib,
}:

import "${root}/programs/agents" {
  inherit pkgs lib;
  externalSrcs.mattpocock = inputs.mattpocock-skills;
}
