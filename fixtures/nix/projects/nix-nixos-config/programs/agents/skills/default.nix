{
  pkgs,
  lib ? pkgs.lib,
  externalSrcs ? { },
  ...
}:

let
  inherit (import ./lib.nix { inherit pkgs lib; })
    selectSource
    mergeSources
    mkSkillFarm
    mkCheck
    ;

  # Third-party skill sets. Each source says where to search and exactly which
  # skills to install — nothing is taken implicitly, so an upstream repo growing
  # new skills never silently changes what lands in ~/.claude/skills.
  #
  # Names are bare: `roots` is walked recursively for SKILL.md directories, so
  # upstream can reorganise its categories without breaking this list. A missing
  # or ambiguous name aborts evaluation.
  #
  # To add a source:
  #   1. declare the repo as a `flake = false` input in flake.nix
  #   2. forward it through lib/agents.nix as `externalSrcs.<name>`
  #   3. add an entry here
  sources = {
    mattpocock = {
      src = externalSrcs.mattpocock;
      roots = [ "skills" ];
      skills = [
        "grill-with-docs" # the entry point
        # "grilling" is forked into ./custom (one question at a time through the
        # picker), because grill-me and grill-with-docs both delegate to it.
        "domain-modeling" # delegated to by grill-with-docs
        "grill-me" # non-code variant of grill-with-docs
        "setup-matt-pocock-skills" # per-repo setup, run once via /setup-matt-pocock-skills
      ];

      # Resolves a collision deliberately — e.g. upstream ships `code-review`,
      # which as a personal skill would shadow Claude Code's bundled
      # /code-review. A rename also rewrites the frontmatter `name`, keeping it
      # in step with the directory that supplies the command name.
      rename = { };
    };
  };

  # Custom skills committed under ./custom. Only names listed here are installed;
  # any other directory stays committed as a draft. See ./custom/README.md.
  custom = [
    "address-pr-feedback" # /address-pr-feedback - answer PR review threads, explicit invocation only
    "engineer" # /engineer - deliver a feature end to end, explicit invocation only
    "engineering" # coding style and engineering principles, model-invoked on any code work
    "explain" # /explain - plain-words explanation of a PR, diff or concept, read-only, explicit invocation only
    "give-feedback" # /give-feedback - apply quick bulleted feedback to a PR in rounds, explicit invocation only
    "grilling" # fork of mattpocock's grilling, one picker question at a time; grill-me and grill-with-docs delegate to it
    "pr-description" # PR body from the repo template or a default format, model-invoked before gh pr create
    "pragmatic-review" # /pragmatic-review - diff and PR review; also invoked by engineer
    "refactor" # /refactor - behaviour-preserving cleanup of a diff; also invoked by engineer
    "skeptic" # /skeptic - fail-fast feasibility review, explicit invocation only
    "ui-coding" # UI-specific rules on top of engineering, model-invoked on UI work
  ];

  # Custom skills go through the same resolution as external sources so they get
  # the same collision detection, rename support and validation.
  customSource = {
    src = ./.;
    roots = [ "custom" ];
    skills = custom;
    rename = { };
  };

  all = mergeSources (
    lib.mapAttrsToList (name: source: {
      inherit name;
      skills = selectSource name source;
    }) sources
    ++ [
      {
        name = "custom";
        skills = selectSource "custom" customSource;
      }
    ]
  );
in
{
  # name -> store path of the skill directory.
  inherit all;

  names = lib.attrNames all;

  # mkSkillFarm <farm-name> <names> — a store directory of symlinks for one
  # Claude flavor to link into ~/.claude/skills.
  mkSkillFarm = mkSkillFarm all;

  check = mkCheck all;
}
