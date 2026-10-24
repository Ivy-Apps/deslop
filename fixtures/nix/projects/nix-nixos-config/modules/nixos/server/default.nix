# A headless NixOS server that runs Claude Code agents for one bot account,
# driven from a phone through Remote Control. Imported only by
# lib.mkNixosServer, so nothing here can reach a desktop host.
#
# Two accounts, two trust levels:
#   myConfig.user - the agent account ("factory"). No sudo. Holds the Claude
#                   login, the bot's GitHub key and token, and the repositories.
#   admin         - wheel, sudo behind a password. Reachable only with the
#                   hardware keys in myConfig.admin.authorizedKeys, for deploys.
#
# See docs/adr/0007-software-factory.md for the threat model.
{ lib, ... }:
{
  imports = [
    ./hetzner.nix
    ./users.nix
    ./ssh.nix
    ./tailscale.nix
    ./egress.nix
    ./nix.nix
    ./resources.nix
    ./nix-ld.nix
    ./claude-policy.nix
    ./ops.nix
  ];

  options.myConfig.factory.repos = lib.mkOption {
    type = lib.types.attrsOf lib.types.str;
    default = { };
    example = {
      nixos = "git@github.com:owner/nixos.git";
    };
    description = ''
      Repositories cloned into ~/repos/<name> and served over Remote Control,
      keyed by directory name. Public repositories only: this list is committed
      to a public repo. Anything private is added on the box with
      `factory-ctl start`. Declared repositories are only bootstrapped: once
      cloned, whether one runs is up to `factory-ctl start` and `stop`.
    '';
  };
}
