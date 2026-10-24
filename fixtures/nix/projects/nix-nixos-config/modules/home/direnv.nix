# Activates a project's flake dev shell on `cd`, which is how project-pinned
# toolchains and LSP binaries reach the PATH.
{ pkgs, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    package = pkgs.direnv.overrideAttrs (_: {
      doCheck = false;
    });
  };
}
