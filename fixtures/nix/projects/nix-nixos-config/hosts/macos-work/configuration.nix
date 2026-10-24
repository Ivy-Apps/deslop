_: {
  networking.hostName = "macos-work";

  # Determinate Nix manages its own daemon — disable nix-darwin's Nix management to avoid conflict.
  nix.enable = false;

  myConfig.user = {
    name = "iliyan-coinlist";
    fullName = "Iliyan Germanov";
    email = "work-user@example.com";
  };

  # Work-only: macos-main deliberately does not provide this, so the Figma MCP
  # server is absent there rather than fed a placeholder token.
  myConfig.secrets.figma-token = { };

  system.stateVersion = 6;
}
