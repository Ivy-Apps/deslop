# Root operations the agent account may trigger without sudo: deploying main
# (pulled from GitHub on a timer), rolling back, rebooting and collecting
# garbage. Each is a fixed unit that takes no input; polkit lets the agent
# account start exactly these and nothing else. Any process of that account can
# start them, which is an accepted availability risk - see
# docs/adr/0007-software-factory.md.
{
  config,
  lib,
  pkgs,
  root,
  ...
}:
let
  cfg = config.myConfig.factory.deploy;
  factory = import "${root}/programs/software-factory" { inherit pkgs lib; };
  stateDir = "/var/lib/factory-ops";

  deploy = factory.mkDeploy {
    inherit (cfg) repo environment;
    inherit stateDir;
    host = config.networking.hostName;
    tokenFile = config.sops.secrets.factory-deploy-token.path;
  };
  maintenance = factory.mkMaintenance { inherit stateDir; };

  # The units the agent account may start.
  agentUnits = [
    "factory-rollback.service"
    "factory-reboot.service"
    "factory-gc.service"
  ];

  # Activation restarts changed units. These would be killed half way through
  # the very switch they are running, so they are left alone.
  oneshot = description: script: {
    inherit description;
    restartIfChanged = false;
    stopIfChanged = false;
    path = [
      config.nix.package
      pkgs.curl
      config.services.tailscale.package
      config.systemd.package
    ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = script;
      StateDirectory = "factory-ops";
      # Readable by the agent account, for `factory-ctl status`.
      StateDirectoryMode = "0755";
    };
  };
in
{
  options.myConfig.factory.deploy = {
    repo = lib.mkOption {
      type = lib.types.str;
      example = "owner/nixos";
      description = "owner/repo of this flake on GitHub, whose deployments the server polls.";
    };
    environment = lib.mkOption {
      type = lib.types.str;
      default = "software-factory";
      description = "The GitHub environment deployments are created for.";
    };
  };

  config = {
    # A fine-grained token of the repository owner: this repository only,
    # Deployments read and write, nothing else. Root-only, unlike the bot's.
    myConfig.secrets.factory-deploy-token = { };

    systemd.services = {
      factory-deploy = oneshot "Deploy the pending GitHub deployment of main" (lib.getExe deploy);
      factory-rollback = oneshot "Roll back to the previous system generation" "${lib.getExe maintenance} rollback";
      factory-reboot = oneshot "Reboot, unless booted less than 10 minutes ago" "${lib.getExe maintenance} reboot";
      factory-gc = oneshot "Keep two generations per profile and collect garbage" "${lib.getExe maintenance} gc";
    };

    systemd.timers = {
      factory-deploy = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "2min";
          OnUnitInactiveSec = "1min";
        };
      };
      factory-gc = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "weekly";
          Persistent = true;
        };
      };
    };

    security.polkit = {
      enable = true;
      extraConfig = ''
        polkit.addRule(function (action, subject) {
          if (action.id == "org.freedesktop.systemd1.manage-units" &&
              subject.user == ${builtins.toJSON config.myConfig.user.name} &&
              action.lookup("verb") == "start" &&
              ${builtins.toJSON agentUnits}.indexOf(action.lookup("unit")) >= 0) {
            return polkit.Result.YES;
          }
        });
      '';
    };

    # Read-only access to the system journal, for /status and the logs of the
    # units above.
    users.users.${config.myConfig.user.name}.extraGroups = [ "systemd-journal" ];
  };
}
