# The agent account may reach the internet - it has to test what it builds -
# but nothing private. The rules live in programs/software-factory, where
# checks.software-factory validates them with nft.
{
  config,
  root,
  pkgs,
  ...
}:
let
  factory = import "${root}/programs/software-factory" { inherit pkgs; };
in
{
  networking.nftables = {
    enable = true;
    tables.factory-egress = {
      family = "inet";
      content = factory.mkEgressRules config.users.users.${config.myConfig.user.name}.uid;
    };
  };
}
