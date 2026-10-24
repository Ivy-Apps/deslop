{
  config,
  lib,
  root,
  ...
}:
let
  factory = config.myConfig.user;
in
{
  imports = [ "${root}/modules/user-options.nix" ];

  options.myConfig.admin.authorizedKeys = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    description = ''
      Public SSH keys allowed to log in as `admin`. Hardware-backed keys only:
      admin is the one account that can become root.
    '';
  };

  config = {
    users = {
      mutableUsers = false;

      users = {
        # The agent account. No password and no wheel: it is reached through
        # Tailscale SSH, which authenticates the tailnet identity instead.
        # Lingering keeps its systemd user manager - and so every Remote
        # Control server - up without anyone logged in. The uid is pinned
        # because the egress rules match on it numerically: a name would need
        # resolving inside the build sandbox, which has no passwd database.
        ${factory.name} = {
          isNormalUser = true;
          uid = 1000;
          description = factory.fullName;
          linger = true;
        };

        admin = {
          isNormalUser = true;
          extraGroups = [ "wheel" ];
          hashedPasswordFile = config.sops.secrets.password-hash.path;
          openssh.authorizedKeys.keys = config.myConfig.admin.authorizedKeys;
        };

        # No password means no console login for root; root is reached via sudo.
        root.hashedPassword = "!";
      };
    };

    # The admin password. sudo asks for it, and so does the Hetzner web console,
    # which is the way back in when the tailnet is down. sshd never accepts it.
    myConfig.secrets.password-hash.neededForUsers = true;

    security.sudo = {
      wheelNeedsPassword = true;
      execWheelOnly = true;
    };

    home-manager.users.${factory.name} = import "${root}/modules/home/factory";
  };
}
