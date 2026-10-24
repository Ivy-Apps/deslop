_: {
  imports = [ ./tailscale.nix ];

  networking.hostName = "macos-main";

  myConfig.user = {
    name = "iliyangermanov";
    fullName = "Iliyan Germanov";
    email = "user@example.com";
  };

  system.stateVersion = 6;
}
