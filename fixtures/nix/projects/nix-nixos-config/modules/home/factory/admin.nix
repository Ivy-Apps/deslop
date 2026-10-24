# ~/admin, the admin agent's working directory: its instructions, permissions
# and skills as a Claude Code project. Kept out of ~/.claude so the repository
# agents of the same account never load them.
{
  root,
  pkgs,
  lib,
  ...
}:
let
  inherit ((import "${root}/programs/software-factory" { inherit pkgs lib; })) admin;
in
{
  home.file = {
    "admin/CLAUDE.md".source = admin.instructions;
    "admin/.claude/settings.json".source = admin.settings;
  }
  // lib.mapAttrs' (
    name: path: lib.nameValuePair "admin/.claude/skills/${name}" { source = path; }
  ) admin.skills;
}
