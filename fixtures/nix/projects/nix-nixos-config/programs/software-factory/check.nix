# checks.software-factory: the factory's policy, scripts and admin skills,
# exercised without a server (the scenarios are in check.sh). git, process
# lookup and the filesystem are real: repositories are cloned from a local bare
# repository, and sessions are real processes. What cannot run in the build
# sandbox is a double - an in-memory systemd, GitHub's API and CLI, Nix's
# build and profile commands, Tailscale - and each records how it was called.
# On Linux the egress rules also go through `nft --check`, the same validation
# NixOS runs when it builds the host.
{
  pkgs,
  lib ? pkgs.lib,
}:

let
  factory = import ./. { inherit pkgs lib; };

  policy = factory.managedSettingsValue;
  policyAssertions = [
    {
      ok = policy.forceLoginMethod == "claudeai";
      msg = "forceLoginMethod must be claudeai, or an API key could bill the API";
    }
    {
      ok = policy.allowedProviders == [ "anthropic" ];
      msg = "allowedProviders must be exactly [ anthropic ]";
    }
    {
      ok = policy.permissions.disableBypassPermissionsMode == "disable";
      msg = "bypassPermissions must be disabled";
    }
    {
      ok = policy.permissions.defaultMode == "auto";
      msg = "sessions must start in auto mode";
    }
  ];
  failedPolicy = lib.filter (a: !a.ok) policyAssertions;

  work = "/tmp/factory-check";

  # Records every call as one line: the program name, then its arguments.
  recorder =
    name:
    pkgs.writeShellScriptBin name ''
      echo "${name} $*" >>"$CALLS"
    '';

  fakeClaude = recorder "claude-factory";
  fakeClaudeUnwrapped = pkgs.writeShellScriptBin "claude" ''
    echo "claude $* GH_TOKEN=$GH_TOKEN PWD=$PWD" >>"$CALLS"
  '';

  remoteControl = factory.mkRemoteControl {
    claude = fakeClaude;
    claudeUnwrapped = fakeClaudeUnwrapped;
    ghToken = "${work}/gh-token";
  };

  ctl = factory.mkCtl {
    opsStateDir = "${work}/ops";
    deployRepo = "owner/nixos";
    deployWorkflow = "deploy-factory.yml";
  };

  boot = factory.mkBoot ctl {
    declared = "file://${work}/origin.git";
    unreachable = "file://${work}/missing.git";
  };

  deploy = factory.mkDeploy {
    repo = "owner/nixos";
    host = "software-factory";
    environment = "software-factory";
    tokenFile = "${work}/deploy-token";
    stateDir = "${work}/ops";
    profile = "${work}/profiles/system";
    currentSystem = "${work}/current-system";
    healthAttempts = 2;
  };

  maintenance = factory.mkMaintenance {
    stateDir = "${work}/ops";
    profile = "${work}/profiles/system";
  };

  # An in-memory systemd: a unit is active while a file named after it exists.
  # Answers `is-active --quiet` for the deploy's health gate from $SSHD_UP.
  fakeSystemctl = pkgs.writeShellScriptBin "systemctl" ''
    echo "systemctl $*" >>"$CALLS"
    [ "$1" != --user ] || shift
    [ "$1" != --no-block ] || shift
    mkdir -p "$UNITS"
    case "$1" in
      start | restart) touch "$UNITS/$2" ;;
      stop) rm -f "$UNITS/$2" ;;
      is-active)
        if [ "$2" = --quiet ]; then [ "''${SSHD_UP:-1}" = 1 ]; exit; fi
        if [ -e "$UNITS/$2" ]; then echo active; else echo inactive; exit 3; fi
        ;;
    esac
  '';

  # GitHub's CLI, for `resolve` and `deploy`. Repositories come from
  # $GH/user-repos.json; a dispatched workflow run gets id 42, and it fails
  # when $GH/run-fails exists.
  fakeGh = pkgs.writeShellScriptBin "gh" ''
    echo "gh $*" >>"$CALLS"
    case "$1 $2" in
      "api --paginate") ${lib.getExe pkgs.jq} -r "$5" "$GH/user-repos.json" ;;
      "workflow run") touch "$GH/dispatched" ;;
      "run list") if [ -e "$GH/dispatched" ]; then echo 42; fi ;;
      "run watch") [ ! -e "$GH/run-fails" ] ;;
      "run view") echo "CI has not passed" ;;
    esac
  '';

  # GitHub's REST API for the deploy script. GETs are answered from files in
  # $GH named after the endpoint; POST bodies are appended to $GH/posted. The
  # curl config arrives on stdin and is kept in $GH/config, apart from argv.
  fakeCurl = pkgs.writeShellScriptBin "curl" ''
    echo "curl $*" >>"$CALLS"
    method=GET headers=/dev/null body="" url=""
    while [ "$#" -gt 0 ]; do
      case "$1" in
        -X) method="$2"; shift ;;
        -D) headers="$2"; shift ;;
        --data-binary) body="''${2#@}"; shift ;;
        -K) cat >"$GH/config"; shift ;;
        -H | -m | -o) shift ;;
        https://*) url="$1" ;;
      esac
      shift
    done
    printf 'HTTP/2 200\r\ngithub-authentication-token-expiration: 2027-01-01 00:00:00 UTC\r\n\r\n' >"$headers"
    case "$method $url" in
      "GET https://api.github.com/zen") [ ! -e "$GH/github-down" ] ;;
      "GET "*/deployments\?*) cat "$GH/deployments.json" ;;
      "GET "*/statuses\?*) cat "$GH/statuses.json" ;;
      "GET "*/commits/main) cat "$GH/main.json" ;;
      "POST "*/statuses) { cat "$body"; echo; } >>"$GH/posted"; echo '{}' ;;
      *) echo "fake curl: unexpected $method $url" >&2; exit 22 ;;
    esac
  '';

  fakeNix = pkgs.writeShellScriptBin "nix" ''
    echo "nix $*" >>"$CALLS"
    [ ! -e "$GH/build-fails" ] && echo "${work}/systems/new"
  '';

  # Profiles are symlinks in ${work}/profiles, as Nix keeps them.
  fakeNixEnv = pkgs.writeShellScriptBin "nix-env" ''
    echo "nix-env $*" >>"$CALLS"
    case "$3" in
      --set) ln -sfn "$4" "$2" ;;
      --rollback) ln -sfn "${work}/systems/old" "$2" ;;
    esac
  '';

  fakeNixStore = pkgs.writeShellScriptBin "nix-store" ''
    echo "nix-store $*" >>"$CALLS"
    [ "$2" != --print-dead ] || echo "$HOME/garbage"
  '';

  fakeTailscale = pkgs.writeShellScriptBin "tailscale" ''
    echo '{"BackendState": "Running"}'
  '';

  skillsCheck = (import ../agents/skills/lib.nix { inherit pkgs lib; }).mkCheck factory.admin.skills;

  nftCheck = lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
    echo "nft --check: egress rules"
    cat >ruleset.conf <<'EOF'
    table inet factory-egress {
    ${factory.mkEgressRules 1000}
    }
    EOF
    # The same invocation NixOS uses for networking.nftables.checkRuleset:
    # an in-process kernel (lkl), with /etc lookups redirected into the store.
    export NIX_REDIRECTS=/etc/protocols=${pkgs.iana-etc}/etc/protocols:/etc/services=${pkgs.iana-etc}/etc/services
    LD_PRELOAD="${pkgs.libredirect}/lib/libredirect.so ${pkgs.lklWithFirewall.lib}/lib/liblkl-hijack.so" \
      ${pkgs.nftables}/bin/nft --check --file ruleset.conf
  '';
in
assert lib.assertMsg (failedPolicy == [ ]) (
  "software-factory managed settings: " + lib.concatMapStringsSep "; " (a: a.msg) failedPolicy
);
pkgs.runCommand "software-factory-check"
  {
    nativeBuildInputs = [
      pkgs.git
      pkgs.jq
      pkgs.procps
      remoteControl.repo
      remoteControl.admin
      ctl
      boot
      deploy
      maintenance
      fakeSystemctl
      fakeGh
      fakeCurl
      fakeNix
      fakeNixEnv
      fakeNixStore
      fakeTailscale
    ];
    inherit work;
    repoArgs = lib.escapeShellArgs factory.remoteControlArgs.repo;
    adminArgs = lib.escapeShellArgs factory.remoteControlArgs.admin;
    adminSettings = factory.admin.settings;
  }
  ''
    set -euo pipefail
    # Admin skills: frontmatter, names and description length.
    echo "admin skills: ${skillsCheck}"

    source ${./check.sh}

    ${nftCheck}

    echo "software-factory: all checks passed"
    touch "$out"
  ''
