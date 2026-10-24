# The Claude Code configuration itself: settings, skills, instructions, the MCP
# catalog and the flavors built from them. Everything mechanical - merging,
# resolving secrets, writing the wrappers - lives in lib.nix, so adding a server
# or a flavor is a change to this file alone.
{
  pkgs,
  lib ? pkgs.lib,
  claude-code,
  agents,
  theme ? "auto",
  secrets ? { },
  ...
}:

let
  inherit
    (import ./lib.nix {
      inherit
        pkgs
        lib
        claude-code
        secrets
        ;
      inherit (agents.skills) mkSkillFarm;
    })
    mkFlavorBuilder
    mkStdioServer
    mkHttpServer
    ;
  statusline = import ./statusline.nix { inherit pkgs theme; };

  baseSettings = {
    statusLine = {
      type = "command";
      command = "${statusline}/bin/claude-statusline";
    };
    autoMemoryEnabled = false;
    effortLevel = "high";
    model = "opus";
    theme = "auto";
  };

  # Local servers name the SECRETS they need, never a path. Which hosts provide
  # them is decided by modules/secrets.nix; a server whose secret this host does
  # not declare is dropped before any flavor sees it, and a flavor naming a
  # server that is not in this catalog at all aborts evaluation.
  #
  # Remote servers name no secrets and are therefore available everywhere: they
  # authenticate out-of-band, once per machine, via `/mcp` inside Claude Code.
  # A host that should not reach one simply does not run a flavor that lists it.
  mcpCatalog = {
    figma = mkStdioServer {
      command = "npx";
      args = [
        "-y"
        "figma-developer-mcp"
        "--stdio"
      ];
      env.FIGMA_API_KEY = "figma-token";
    };

    # Read-write. Linear also serves /mcp/readonly, but the point of claude-pm
    # is filing and updating issues, not only reading them.
    linear = mkHttpServer { url = "https://mcp.linear.app/mcp"; };

    # Coda's docs product, renamed after the Superhuman acquisition. The old
    # coda.io/apis/mcp endpoint still answers but is documented as deprecated.
    superhuman-docs = mkHttpServer { url = "https://docs.superhuman.com/apis/mcp"; };
  };

  # The agent-agnostic instructions, installed the way Claude Code expects them:
  # as user-scope memory at ~/.claude/CLAUDE.md. Claude Code does not read
  # AGENTS.md at user scope, so the file is copied under the name it does read.
  # A project's own CLAUDE.md still stacks on top of this - user and project
  # memory are separate entries, not competing ones.
  baseInstructions = agents.instructions;

  # Skills every flavor gets. Names must be installed by programs/agents/skills -
  # requesting an unknown one aborts evaluation. Flavor-specific additions go in
  # that flavor's `extraSkills`.
  baseSkills = [
    "address-pr-feedback"
    "engineer"
    "engineering"
    "explain"
    "give-feedback"
    "grill-with-docs"
    "grilling"
    "domain-modeling"
    "grill-me"
    "pr-description"
    "pragmatic-review"
    "refactor"
    "setup-matt-pocock-skills"
    "skeptic"
    "ui-coding"
  ];

  mkClaudeFlavors = mkFlavorBuilder {
    inherit
      baseSettings
      baseSkills
      baseInstructions
      mcpCatalog
      ;
  };

  baseSettingsFile = pkgs.writeText "claude-base-settings.json" (builtins.toJSON baseSettings);
in
{
  # One entry per flavor; the attr name is the binary name. Everything is
  # optional - `claude` is the base configuration with nothing added.
  packages = mkClaudeFlavors {
    claude = { };

    claude-ts.extraSettings.enabledPlugins = {
      "typescript-lsp@claude-plugins-official" = true;
    };

    claude-web-ui = {
      mcpServers = [ "figma" ];
      extraSettings.enabledPlugins = {
        "typescript-lsp@claude-plugins-official" = true;
        "frontend-design@claude-plugins-official" = true;
      };
    };

    # claude-web-ui plus the product-work servers. Spelled out rather than
    # derived from claude-web-ui: flavors are flat by design, and one shared
    # `let` binding for a single pair would hide which binary gets what.
    claude-pm = {
      mcpServers = [
        "figma"
        "linear"
        "superhuman-docs"
      ];
      extraSettings.enabledPlugins = {
        "typescript-lsp@claude-plugins-official" = true;
        "frontend-design@claude-plugins-official" = true;
      };
    };
  };

  # The software-factory server's flavor, kept out of `packages` so no other host
  # installs it. Its settings are only what a flavor may own: policy that must
  # not be editable by the agents themselves lives in the server's managed
  # settings (programs/software-factory).
  #
  # `unwrapped` is the binary the wrapper execs. The factory launches the wrapper
  # once under a lock to apply its files, then starts every Remote Control
  # server from this binary, so concurrent starts cannot race on ~/.claude.
  factory = {
    wrapper = builtins.head (mkClaudeFlavors {
      claude-factory = {
        extraInstructions = [ ../software-factory/instructions.md ];
        extraSettings.enabledPlugins = {
          "typescript-lsp@claude-plugins-official" = true;
        };
      };
    });
    unwrapped = claude-code;
  };

  # Writes the base settings and instructions on every rebuild so both files
  # exist before the first `claude` invocation. Each wrapper then overwrites
  # them at launch time.
  #
  # `install -m 600`, not `cp`: store paths are mode 444, so copying one over a
  # previous copy of itself fails with EACCES. `install` sets the mode on the
  # destination it writes, which makes the operation repeatable.
  activationScript = ''
    mkdir -p "$HOME/.claude"
    install -m 600 ${baseSettingsFile} "$HOME/.claude/settings.json"
    install -m 600 ${baseInstructions} "$HOME/.claude/CLAUDE.md"
  '';
}
