{ lib, ... }:
{
  imports = [ ./disk-config.nix ];

  networking.hostName = "software-factory";

  myConfig = {
    # The agent account commits as the bot, which has write access to the
    # repositories but cannot merge into their protected main branches.
    user = {
      name = "factory";
      fullName = "IvyBot";
      email = "bot@example.com";
    };

    # macos-main's YubiKey-backed keys, primary and backup.
    admin.authorizedKeys = [
      "ssh-ed25519 AAAAprimaryfixture"
      "ssh-ed25519 AAAAbackupfixture"
    ];

    factory = {
      # Hetzner CX33 (4 vCPU, 8 GB). Switch to "standard" on an 8 vCPU / 16 GB
      # machine.
      size = "medium";

      # Deploys are pulled from this repository (modules/nixos/server/ops.nix).
      deploy.repo = "ILIYANGERMANOV/nixos";

      # Public repositories only - private ones are added on the box with
      # `factory-ctl start` and never named here.
      repos = {
        nixos = "git@github.com:ILIYANGERMANOV/nixos.git";
        deslop = "git@github.com:Ivy-Apps/deslop.git";
        haskell-template = "git@github.com:Ivy-Apps/haskell-template.git";
      };
    };

    secrets = {
      gh-token = { };

      # modules/secrets.nix declares this probe as global on every host. This
      # host's age key is not a recipient of secrets/common.yaml, so the probe
      # reads the same key from the host's own file instead.
      example-global.scope = lib.mkForce "host";
    };
  };

  system.stateVersion = "26.05";
}
