{
  config,
  lib,
  pkgs,
  root,
  ...
}:
let
  cfg = config.myConfig.secrets;

  # Where a secret's ciphertext lives, derived from its scope.
  #
  # `scope` is an explicit field rather than something inferred from where the
  # declaration was written: the module system merges every definition into one
  # attrset and erases its origin, so "declared in a host file" is not
  # information that survives to evaluation time.
  fileFor =
    scope:
    if scope == "global" then
      "${root}/secrets/common.yaml"
    else
      "${root}/secrets/hosts/${config.networking.hostName}.yaml";

  # sops-nix places neededForUsers secrets in /run/secrets-for-users before any
  # user account exists, so they must stay root-owned; passing owner/mode there
  # trips sops-nix's own assertion. Every other secret gets this host's primary
  # user, which is the only reason `owner` was ever written out by hand.
  toSopsSecret =
    _: s:
    {
      sopsFile = fileFor s.scope;
    }
    // (if s.neededForUsers then { neededForUsers = true; } else { inherit (s) owner mode; });
in
{
  options.myConfig.secrets = lib.mkOption {
    default = { };
    description = ''
      The secrets this host provides.

      Declaring a secret here is the host's promise that a key of the same name
      exists in the matching sops file. Nothing else declares secrets: this
      registry is the only door to `sops.secrets`.

      Config that depends on a secret must key off its presence and disable
      itself when it is absent, rather than assume every host has it. That is
      what lets a work-only credential stay work-only instead of forcing a
      placeholder value onto every other machine.
    '';
    example = lib.literalExpression ''
      {
        figma-token = { };                              # host-scoped (the default)
        shared-thing = { scope = "global"; };            # every host
      }
    '';
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          scope = lib.mkOption {
            type = lib.types.enum [
              "global"
              "host"
            ];
            default = "host";
            description = ''
              "global" - every host has it; read from `secrets/common.yaml`.
              "host"   - only hosts that opt in; read from
                         `secrets/hosts/<hostname>.yaml`.
            '';
          };

          owner = lib.mkOption {
            type = lib.types.str;
            default = config.myConfig.user.name;
            defaultText = lib.literalExpression "config.myConfig.user.name";
            description = "User allowed to read the decrypted file.";
          };

          mode = lib.mkOption {
            type = lib.types.str;
            default = "0400";
            description = ''
              Octal permission on the decrypted file. One digit per class
              (owner, group, other) summing read (4), write (2) and execute (1).
              The default 0400 is read-only for `owner` and unreadable to
              everyone else.
            '';
          };

          neededForUsers = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = ''
              NixOS only: decrypt before user accounts are created, so the value
              can be used as a `hashedPasswordFile`. Forces root ownership.
            '';
          };
        };
      }
    );
  };

  config = {
    sops = {
      defaultSopsFormat = "yaml";
      age.keyFile = "/var/lib/sops-age/keys.txt";

      # No defaultSopsFile: every secret names its own file via `scope`.
      secrets = lib.mapAttrs toSopsSecret cfg;
    };

    # Exists so the global scope is exercised on every host on every rebuild.
    # If `secrets/common.yaml` or the scope derivation is ever wrong, that fails
    # here rather than months later when the first real shared secret lands.
    # Nothing reads this value, and nothing should.
    myConfig.secrets.example-global.scope = "global";

    assertions = [
      {
        assertion =
          pkgs.stdenv.hostPlatform.isDarwin -> !(lib.any (s: s.neededForUsers) (lib.attrValues cfg));
        # `neededForUsers` is a NixOS concept; nix-darwin has no user-activation
        # phase to decrypt ahead of, and sops-nix's darwin module ignores it.
        message = "myConfig.secrets: neededForUsers is NixOS-only, but a secret sets it on a Darwin host.";
      }
    ]
    ++ lib.mapAttrsToList (name: s: {
      assertion = builtins.pathExists (fileFor s.scope);
      message = ''
        myConfig.secrets.${name} (scope "${s.scope}") expects ${fileFor s.scope}, which does not exist.
        Create it with: just edit-secrets ${if s.scope == "global" then "" else config.networking.hostName}
      '';
    }) cfg;
  };
}
