{
  root,
  pkgs,
  lib,
  config,
  ...
}:
let
  typos = import "${root}/programs/typos" { inherit pkgs lib; };
in
{
  home.packages = [ typos.cli ];

  # The global allowlist is intentionally NOT Home Manager managed. It holds words
  # that must not be committed to this public repo, and <leader>ad has to be able
  # to append to it — both rule out a read-only store path. Seed it once when it
  # is missing, then never touch it again. See programs/typos for the full
  # rationale.
  home.activation.typosGlobalAllowlist = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    typosAllowlist="${config.home.homeDirectory}/${typos.globalConfigRelPath}"
    if [ ! -e "$typosAllowlist" ]; then
      run mkdir -p "$(dirname "$typosAllowlist")"
      # 0600: this file is expected to accumulate words from private work.
      run install -m 0600 ${typos.seed} "$typosAllowlist"
    fi
  '';
}
