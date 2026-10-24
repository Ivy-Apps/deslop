# Nix for a machine that runs untrusted builds on behalf of agents.
# Build parallelism and disk headroom are sized in resources.nix.
{ config, lib, ... }:
{
  nix = {
    settings = {
      # modules/nix.nix allows "relaxed" for a darwin package that needs the
      # network. Here every build is sandboxed, without exceptions.
      sandbox = lib.mkForce true;

      # The agent account may use the daemon but is never trusted: a trusted
      # user can add substituters, import unsigned paths and turn the sandbox
      # off, which amounts to root. Deploys copy only content-addressed paths
      # (sources and .drv files), which needs no trust either.
      allowed-users = [
        "root"
        "@wheel"
        config.myConfig.user.name
      ];
      trusted-users = [ "root" ];

      # Agents enter project dev shells whose flakes carry a nixConfig, and
      # without this every `nix develop` has to be told to accept it. Safe
      # because the agent account is untrusted: it gains only what it could
      # already pass as --option, and the daemon still ignores any substituter
      # a flake names that is not already trusted here.
      accept-flake-config = true;

      # Caches the agent account may use when a flake asks for them. Unlike
      # extra-substituters they are queried only by builds whose flake names
      # them, so every other build is spared the lookups. The keys must be
      # trusted here too: a flake's own extra-trusted-public-keys is ignored for
      # an untrusted user.
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://cache.iog.io"
      ];
      extra-trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
    };

    # Garbage is collected by factory-gc (ops.nix), which keeps exactly the two
    # generations a rollback can reach instead of an age cut-off.
    gc.automatic = false;
  };

  # The boot menu offers the same two generations.
  boot.loader.grub.configurationLimit = 2;
}
