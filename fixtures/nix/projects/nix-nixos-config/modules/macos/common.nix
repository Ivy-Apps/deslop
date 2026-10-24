{ config, ... }: {
  # Only root and the primary user may push store paths to the daemon,
  # not every admin account (was "@admin").
  nix.settings.trusted-users = [
    "root"
    config.myConfig.user.name
  ];

  # Enable Touch ID for sudo (macOS only)
  security.pam.services.sudo_local.touchIdAuth = true;

  # Application firewall: drop unsolicited inbound connections, and stay
  # invisible to probes/pings (stealth mode).
  networking.applicationFirewall = {
    enable = true;
    enableStealthMode = true;
  };

  # Require the password immediately when the screen sleeps / screensaver starts.
  system.defaults.screensaver = {
    askForPassword = true;
    askForPasswordDelay = 0;
  };

  # nix-darwin requires zsh to be enabled at the system level for login shells
  programs.zsh.enable = true;

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
    };
    casks = [
      "ghostty"
    ];
  };
}
