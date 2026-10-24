# Source-code spell checking — https://github.com/crate-ci/typos
#
# typos only flags *known* misspellings rather than unknown words, which is what
# makes it tolerable as an always-on editor diagnostic. The false positives it
# does produce are mostly tool names that collide with real misspellings
# (`noice` -> `noise`) and short tokens in non-prose contexts.
#
# There are two allowlist layers, and the split is about disclosure, not scope:
#
#   * a committed `.typos.toml` per project — shared, reviewable, and the only
#     layer CI ever sees.
#   * `~/.config/typos/typos.toml` — mutable, untracked, for words that must not
#     be disclosed (client names, internal codenames encountered while working in
#     private repos). This nixos repo is public, so a Nix-built config is not an
#     option: it would have to be generated from something committed here, which
#     is the exact thing the layer exists to avoid. It is also the only shape the
#     `<leader>ad` keymap can append to.
#
# The layers genuinely compose. `--config` is loaded into typos' *override* layer
# rather than replacing discovery (typos-cli/src/bin/typos-cli/main.rs), and
# `update()` extends collections, so `extend-words` from both layers merge.
#
# That explicit channel is also the *only* mechanism that works. typos' ancestor
# walk breaks at the first config it finds:
#
#     if !self.isolated {
#         for ancestor in cwd.ancestors() {
#             if let Some(derived) = Config::from_dir(ancestor)? {
#                 config.update(&derived);
#                 break;
#             }
#         }
#     }                                        -- typos-cli/src/policy.rs
#
# so simply dropping a file in $HOME would appear to work and then silently stop
# the moment a project grew a `.typos.toml` of its own.
{
  pkgs,
  lib ? pkgs.lib,
  # Where the mutable global allowlist lives, relative to $HOME. Deliberately
  # kept out of the ancestor-walk path: it must only ever apply when passed
  # explicitly, never by accidental discovery from a nearby directory.
  globalConfigRelPath ? ".config/typos/typos.toml",
}:
let
  typos = lib.getExe' pkgs.typos "typos";
in
{
  inherit globalConfigRelPath;

  # `~`-relative form, for consumers that expand it themselves (typos-lsp does).
  globalConfigPath = "~/${globalConfigRelPath}";

  # Written once by home activation, only when the file is absent. Because the
  # file is user-owned and never overwritten, this is a one-time seed rather than
  # a managed value: later changes here do not reach already-provisioned machines.
  seed = pkgs.writeText "typos-global-allowlist.toml" ''
    # Mutable, untracked global typos allowlist.
    #
    # Seeded once by Home Manager and never overwritten, so hand edits are safe —
    # but improvements to the seed in the nixos repo will not reach this file.
    # Add words from Neovim with <leader>ad, or edit it directly.
    #
    # Only put words here that must NOT be committed to a public repo — client
    # names, internal codenames. Anything shareable belongs in the project's own
    # .typos.toml, where CI and teammates can see it.
    #
    # Keys map a word to itself to mark it valid. Matching is case-insensitive and
    # happens after identifiers are split, so one lowercase entry covers every
    # casing and every identifier the word appears inside.

    [files]
    # Machine-generated or encoded content is never meaningfully spell-checkable,
    # and regenerating it reshuffles the letters — word entries would not survive
    # the next rotation, so these have to be excluded by path.
    extend-exclude = [
      "*.lock",
      "*.age",
      "*.enc.*",
      "secrets/**",
    ]

    [default.extend-words]
  '';

  # `typos` for $PATH, carrying the global layer.
  #
  # The existence check is load-bearing: typos exits 78 (EX_CONFIG) on *every*
  # invocation when --config points at a missing file, so a deleted allowlist has
  # to degrade to plain typos rather than break the command outright.
  cli = pkgs.writeShellApplication {
    name = "typos";
    text = ''
      config="$HOME/${globalConfigRelPath}"
      if [ -f "$config" ]; then
        exec ${typos} --config "$config" "$@"
      fi
      exec ${typos} "$@"
    '';
  };

  # The `nix flake check` gate.
  #
  # Pinned to the locked nixpkgs on purpose. typos' dictionary *is* the product
  # and grows with every release, so a floating version could turn CI red with no
  # change to the repo at all. Runs unwrapped, so it sees only the committed
  # `.typos.toml` — never the private global layer.
  mkCheck =
    src:
    pkgs.runCommand "typos-check" { nativeBuildInputs = [ pkgs.typos ]; } ''
      cd ${src}
      typos --color always
      touch "$out"
    '';
}
