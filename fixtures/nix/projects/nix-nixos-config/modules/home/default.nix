{ pkgs, userConfig, ... }: {
  imports = [
    ./nvim.nix
    ./terminal.nix
    ./herdr.nix
    ./claude-code.nix
    ./bitwarden.nix
    ./git.nix
    ./ssh.nix
    ./typos.nix
    ./direnv.nix
    ./blockchain.nix
    ./languages/typescript.nix
    ./languages/haskell.nix
    ./languages/nix.nix
    ./languages/yaml.nix
    ./languages/d2.nix
  ];

  home = {
    username = userConfig.name;
    homeDirectory =
      if pkgs.stdenv.isDarwin then "/Users/${userConfig.name}" else "/home/${userConfig.name}";
    stateVersion = "25.11";
    packages = with pkgs; [
      wget
      curl
      htop
      just
      websocat # test WebSocket endpoints from the terminal (wscat alternative)
      age # inspect / validate the SOPS age key
      sops # edit secrets/ (just edit-secrets)
    ];
  };
}
