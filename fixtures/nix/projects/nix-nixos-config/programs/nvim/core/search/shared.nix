# Search policy shared by every picker in this directory.
#
# A plain function rather than a nixvim module: `file.nix` and `grep.nix`
# `import` it directly instead of listing it in `imports`, because a module
# cannot export helpers. Same convention as the `programs/` tree at the repo
# root.
{ lib }:

let
  # Directories no picker should ever descend into. Pruning at traversal time
  # (vs. filtering results after the fact) is what keeps the pickers instant in
  # large JS/Nix repos. `.direnv` matters here specifically because nix-direnv
  # drops big GC-root trees into every project.
  excludeDirs = [
    "node_modules"
    ".git"
    ".direnv"
    "dist"
    "build"
    "target"
    ".next"
    "coverage"
  ];

  # Telescope's `file_ignore_patterns` are Lua patterns, not globs: `.` and `-`
  # are metacharacters there, so an unescaped `.next` would also match `Xnext`.
  # `%` is listed first because `replaceStrings` makes a single left-to-right
  # pass and never rescans what it just wrote - escaping it last would corrupt
  # every escape produced before it.
  escapeLuaPattern =
    lib.replaceStrings
      [
        "%"
        "^"
        "$"
        "("
        ")"
        "."
        "["
        "]"
        "*"
        "+"
        "-"
        "?"
      ]
      [
        "%%"
        "%^"
        "%$"
        "%("
        "%)"
        "%."
        "%["
        "%]"
        "%*"
        "%+"
        "%-"
        "%?"
      ];
in
{
  inherit excludeDirs;

  # `fd --exclude <dir>`, which matches the bare name at any depth.
  fdExcludeFlags = lib.concatMap (d: [
    "--exclude"
    d
  ]) excludeDirs;

  # The same policy expressed for telescope. Two patterns per directory so a
  # name only matches as a whole path segment: a bare `build/` would also hit
  # `src/rebuild/`, while anchoring with `^` alone would miss the nested copies
  # that `fd --exclude` does prune.
  ignorePatterns = lib.concatMap (
    d:
    let
      e = escapeLuaPattern d;
    in
    [
      "^${e}/"
      "/${e}/"
    ]
  ) excludeDirs;

  # Render a Nix list of strings as a Lua array literal: { "a", "b" }
  toLuaList = xs: "{ " + lib.concatStringsSep ", " (map (x: ''"${x}"'') xs) + " }";
}
