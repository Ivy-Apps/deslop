{
  pkgs,
  lib ? pkgs.lib,
  ...
}:

let

  # Recursively walks `dir`, returning every subdirectory that directly contains
  # a SKILL.md as { name; relative; path; }.
  #
  # `dir` must be a plain store path — a flake input declared with `flake = false`,
  # or a path inside this repo. It must never be a derivation output: readDir on
  # one is import-from-derivation, which modules/nix.nix disables.
  collect =
    dir: prefix:
    let
      entries = builtins.readDir dir;

      # "symlink" entries are accepted as leaves but never recursed into, so a
      # cyclic symlink in an upstream repo cannot hang evaluation.
      candidates = lib.attrNames (
        lib.filterAttrs (_: type: type == "directory" || type == "symlink") entries
      );

      step =
        name:
        let
          sub = dir + "/${name}";
          relative = if prefix == "" then name else "${prefix}/${name}";
        in
        if builtins.pathExists (sub + "/SKILL.md") then
          [
            {
              inherit name relative;
              path = sub;
            }
          ]
        else if entries.${name} == "directory" then
          collect sub relative
        else
          [ ];
    in
    lib.concatMap step candidates;

  # name -> [ entry ] across every root of a source. Grouping rather than
  # flattening means a name defined twice can be reported with both locations
  # instead of silently resolving to whichever was walked first.
  indexSource =
    source:
    lib.groupBy (entry: entry.name) (
      lib.concatMap (root: collect (source.src + "/${root}") root) source.roots
    );

  # A renamed skill cannot be a bare symlink. Claude Code takes the command name
  # from the directory name, so a rename must also rewrite the frontmatter `name`
  # — inserting one if absent — or the skill's listed name and its command would
  # disagree. Skills that are not renamed stay pure symlinks into the source and
  # cost nothing to build.
  renameSkill =
    {
      from,
      to,
      path,
    }:
    if from == to then
      path
    else
      pkgs.runCommand "skill-${to}" { } ''
        cp -r --no-preserve=mode,ownership ${path} "$out"
        chmod -R u+w "$out"
        awk -v new=${lib.escapeShellArg to} '
          NR == 1 && $0 == "---" { infm = 1; print; next }
          infm && $0 == "---"    { if (!seen) print "name: " new; infm = 0; print; next }
          infm && /^name:/       { print "name: " new; seen = 1; next }
                                 { print }
        ' "$out/SKILL.md" > "$out/SKILL.md.new"
        mv "$out/SKILL.md.new" "$out/SKILL.md"
      '';

  # Resolves one source's allowlist into an installed `name -> path` attrset,
  # applying its rename map. Both failure modes abort evaluation with the
  # information needed to fix the catalog.
  selectSource =
    sourceName: source:
    let
      index = indexSource source;
      available = lib.concatStringsSep ", " (lib.naturalSort (lib.attrNames index));

      resolve =
        name:
        let
          hits = index.${name} or [ ];
        in
        if hits == [ ] then
          throw ''
            skill '${name}' not found in source '${sourceName}'
              available: ${available}
          ''
        else if lib.length hits > 1 then
          throw ''
            skill '${name}' is ambiguous in source '${sourceName}'
              found at: ${lib.concatMapStringsSep ", " (hit: hit.relative) hits}
              narrow `roots`, or pick one with a `rename` entry
          ''
        else
          (lib.head hits).path;

      install =
        name:
        let
          installed = source.rename.${name} or name;
        in
        lib.nameValuePair installed (renameSkill {
          from = name;
          to = installed;
          path = resolve name;
        });
    in
    lib.listToAttrs (map install source.skills);

  # Merges per-source attrsets into one namespace, aborting on any collision.
  # Skills are installed flat into ~/.claude/skills, so two sources claiming one
  # name would silently shadow each other on disk.
  mergeSources =
    sets:
    (lib.foldl'
      (
        acc: entry:
        let
          clashes = lib.attrNames (lib.intersectAttrs acc.skills entry.skills);
          clash = lib.head clashes;
        in
        if clashes != [ ] then
          throw ''
            skill '${clash}' is defined by both '${acc.owners.${clash}}' and '${entry.name}'
              add a `rename` entry to one of them, or drop it from an allowlist
          ''
        else
          {
            skills = acc.skills // entry.skills;
            # Tracks which source each name came from, so a collision names the
            # actual pair rather than whichever source was merged last.
            owners = acc.owners // lib.mapAttrs (_: _: entry.name) entry.skills;
          }
      )
      {
        skills = { };
        owners = { };
      }
      sets
    ).skills;

  # A store directory of symlinks, one per skill, consumed by the Claude flavor
  # wrappers. linkFarm is exactly this; hand-rolling a runCommand would not be.
  mkSkillFarm =
    skills: farmName: names:
    let
      missing = lib.filter (name: !(skills ? ${name})) names;
    in
    if missing != [ ] then
      throw ''
        skill farm '${farmName}' requests uninstalled skill(s): ${lib.concatStringsSep ", " missing}
          installed: ${lib.concatStringsSep ", " (lib.attrNames skills)}
      ''
    else
      pkgs.linkFarm farmName (
        map (name: {
          inherit name;
          path = skills.${name};
        }) names
      );

  # Validates every installed skill at build time, wired into `nix flake check`
  # (and therefore `just check` in CI). Duplicate names are caught earlier during
  # evaluation, so this only covers what requires reading the file.
  mkCheck =
    skills:
    pkgs.runCommand "skills-check" { nativeBuildInputs = [ pkgs.yq-go ]; } ''
      status=0

      fail() {
        echo "  ✗ $1" >&2
        echo "      $2" >&2
        status=1
      }

      check_skill() {
        local name="$1" dir="$2" md fm fmname desc len

        md="$dir/SKILL.md"
        if [ ! -f "$md" ]; then
          fail "$name" "no SKILL.md in $dir"
          return
        fi

        if [ "$(head -n 1 "$md")" != "---" ]; then
          fail "$name" "SKILL.md does not open with a YAML frontmatter block"
          return
        fi

        # Everything between the first two --- lines.
        fm=$(awk 'NR == 1 && $0 == "---" { next } $0 == "---" { exit } { print }' "$md")
        fmname=$(printf '%s\n' "$fm" | yq -r '.name // ""')
        desc=$(printf '%s\n' "$fm" | yq -r '.description // ""')

        # The directory name is what Claude Code uses as the command name, so the
        # frontmatter has to agree with it.
        if [ -z "$fmname" ]; then
          fail "$name" "frontmatter is missing required field 'name'"
        elif [ "$fmname" != "$name" ]; then
          fail "$name" "frontmatter name '$fmname' does not match installed directory '$name'"
        fi

        case "$name" in
          *[!a-z0-9-]* | -* | *- | *--*)
            fail "$name" "name must match ^[a-z0-9]+(-[a-z0-9]+)*$"
            ;;
        esac

        if [ -z "$desc" ]; then
          fail "$name" "frontmatter is missing required field 'description'"
        else
          len=$(printf '%s' "$desc" | wc -m | tr -d '[:space:]')
          if [ "$len" -gt 1024 ]; then
            fail "$name" "description is $len characters; the Agent Skills limit is 1024"
          fi
        fi
      }

      ${lib.concatStrings (
        lib.mapAttrsToList (name: path: ''
          check_skill ${lib.escapeShellArg name} ${path}
        '') skills
      )}

      if [ "$status" -ne 0 ]; then
        echo "" >&2
        echo "skills check failed - see programs/agents/skills/default.nix" >&2
        exit 1
      fi

      echo "checked ${toString (lib.length (lib.attrNames skills))} skill(s)" > "$out"
    '';

in
{
  inherit
    selectSource
    mergeSources
    mkSkillFarm
    mkCheck
    ;
}
