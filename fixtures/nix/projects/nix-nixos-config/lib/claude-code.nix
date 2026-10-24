{
  root,
  pkgs,
  inputs,
  lib ? pkgs.lib,
  theme ? "auto",
  # name -> decrypted-file path, for the secrets THIS host provides. Paths only,
  # never values. Servers whose secret is absent are dropped from the catalog.
  secrets ? { },
}:

import "${root}/programs/claude-code" {
  inherit
    pkgs
    lib
    theme
    secrets
    ;
  claude-code = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code;
  agents = import "${root}/lib/agents.nix" {
    inherit
      root
      pkgs
      inputs
      lib
      ;
  };
}
