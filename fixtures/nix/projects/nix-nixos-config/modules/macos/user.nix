{
  config,
  root,
  ...
}:
let
  cfg = config.myConfig.user;
in
{
  imports = [ "${root}/modules/user-options.nix" ];

  config = {
    system.primaryUser = cfg.name;
    users.users.${cfg.name} = {
      home = "/Users/${cfg.name}";
    };
    home-manager.users.${cfg.name} = import "${root}/modules/home/default.nix";
  };
}
