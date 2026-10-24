# The Remote Control servers: the admin agent's, always on, and one per enabled
# repository in ~/repos as instances of the claude-rc@ user unit, plus the boot
# unit that clones the declared repositories and starts the enabled ones.
{
  root,
  inputs,
  pkgs,
  lib,
  config,
  osConfig,
  themeConfig,
  secretsConfig,
  ...
}:

let
  claude = import "${root}/lib/claude-code.nix" {
    inherit
      root
      pkgs
      inputs
      lib
      ;
    theme = themeConfig;
    secrets = secretsConfig;
  };
  factory = import "${root}/programs/software-factory" { inherit pkgs lib; };

  remoteControl = factory.mkRemoteControl {
    claude = claude.factory.wrapper;
    claudeUnwrapped = claude.factory.unwrapped;
    ghToken =
      secretsConfig.gh-token
        or (throw "software-factory: the host must declare myConfig.secrets.gh-token for gh");
  };

  ctl = factory.mkCtl {
    opsStateDir = "/var/lib/factory-ops";
    deployRepo = osConfig.myConfig.factory.deploy.repo;
    deployWorkflow = "deploy-factory.yml";
  };

  # A systemd unit's PATH holds only systemd's own directories. Agents need
  # this account's profile and the system's, including setuid wrappers.
  path = lib.concatStringsSep ":" [
    "/etc/profiles/per-user/${config.home.username}/bin"
    "/run/wrappers/bin"
    "/run/current-system/sw/bin"
  ];

  serverService = {
    Environment = [ "PATH=${path}" ];
    # Belt and braces with forceLoginMethod in the managed settings: nothing
    # may steer a session towards API billing or another endpoint.
    UnsetEnvironment = [
      "ANTHROPIC_API_KEY"
      "ANTHROPIC_AUTH_TOKEN"
      "ANTHROPIC_BASE_URL"
    ];
  };

  # A deploy must not end the sessions it is running under, the admin
  # session that requested it included. Servers pick up a new configuration
  # when they are next restarted (factory-ctl restart, or a reboot).
  keepOld.X-SwitchMethod = "keep-old";
in
{
  imports = [ ./admin.nix ];

  home.packages = [
    claude.factory.wrapper
    ctl
  ];

  systemd.user.services = {
    "claude-rc@" = {
      Unit = keepOld // {
        Description = "Claude Code Remote Control for ~/repos/%i";
      };
      Service = serverService // {
        ExecStart = "${lib.getExe remoteControl.repo} %i";
        Restart = "on-failure";
        RestartSec = 30;
      };
    };

    # The person's way into the box, so it is never off: outside the enabled
    # set factory-ctl manages, and always restarted. MemoryMin keeps its pages
    # from being reclaimed under pressure. oomd kills the heaviest cgroup, which
    # is a build rather than this small server; ManagedOOMPreference would not
    # help, as oomd ignores it on a user unit under a root-owned slice.
    claude-rc-admin = {
      Unit = keepOld // {
        Description = "Claude Code Remote Control for the factory admin agent";
      };
      Service = serverService // {
        ExecStart = lib.getExe remoteControl.admin;
        Restart = "always";
        RestartSec = 10;
        MemoryMin = "512M";
      };
      Install.WantedBy = [ "default.target" ];
    };

    factory-repos = {
      Unit.Description = "Clone the declared repositories and start the enabled Remote Control servers";
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = lib.getExe (factory.mkBoot ctl osConfig.myConfig.factory.repos);
        Environment = [ "PATH=${path}" ];
        # A clone fails until the bot's key exists and the network is up.
        Restart = "on-failure";
        RestartSec = 60;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
