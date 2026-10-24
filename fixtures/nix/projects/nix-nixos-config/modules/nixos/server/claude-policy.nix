# Claude Code's managed settings: root-owned, so no agent can edit them, and
# they outrank every user, project and command-line setting.
{ root, pkgs, ... }:
let
  factory = import "${root}/programs/software-factory" { inherit pkgs; };
in
{
  environment.etc."claude-code/managed-settings.json".source = factory.managedSettings;
}
