# Home Manager profile of the software factory's agent account. It reuses the
# editor and language tooling every host has, and replaces what is tied to a
# person at a keyboard: git and SSH use the bot account's plain key instead of
# a YubiKey, and Claude Code runs as Remote Control servers instead of a CLI.
{ pkgs, userConfig, ... }:
{
  imports = [
    ../nvim.nix
    ../typos.nix
    ../languages/typescript.nix
    ../languages/haskell.nix
    ../languages/nix.nix
    ./git.nix
    ./ssh.nix
    ./claude-factory.nix
  ];

  home = {
    username = userConfig.name;
    homeDirectory = "/home/${userConfig.name}";
    stateVersion = "26.05";
    # What agents reach for outside a project's dev shell: scripting, search,
    # archives and inspecting files and processes.
    packages = with pkgs; [
      wget
      curl
      htop
      jq
      just
      python3
      ripgrep
      fd
      tree
      file
      unzip
      zip
      gnumake
      sqlite
      yq-go
      xxd
      bc
      lsof
    ];
  };
}
