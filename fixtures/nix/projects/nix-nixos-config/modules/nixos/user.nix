{
  config,
  lib,
  root,
  ...
}:
let
  cfg = config.myConfig.user;
in
{
  imports = [ "${root}/modules/user-options.nix" ];

  options.myConfig.user = {
    extraGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "wheel"
        "networkmanager"
        "video"
        "audio"
      ];
      description = "Groups the user belongs to";
    };
  };

  config = {
    # The per-host secrets file already scopes this to one machine, so the
    # value needs no username prefix. It is a yescrypt hash, not a password.
    myConfig.secrets.password-hash = {
      neededForUsers = true;
    };

    users.mutableUsers = false;
    users.users.${cfg.name} = {
      isNormalUser = true;
      description = cfg.fullName;
      inherit (cfg) extraGroups;
      hashedPasswordFile = config.sops.secrets.password-hash.path;
    };

    home-manager.users.${cfg.name} = import "${root}/modules/home/default.nix";
  };
}
