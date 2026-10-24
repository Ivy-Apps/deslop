# Content search. One engine owns all of it: snacks.picker.
#
# What this replaced: <leader>fd and <leader>fg were both bound to telescope's
# live_grep_args - two keys, one identical picker. See
# docs/adr/0003-picker-engines.md.
#
# Stock snacks, with three settings. Everything else is an upstream default:
#
#   - Live ripgrep as you type, in the default centred dialog with a preview.
#   - Raw ripgrep flags after a ` -- ` separator: `useQuery -- -t ts`.
#   - <C-g> freezes the results and turns the prompt into a fuzzy filter over
#     them, with fzf syntax: 'exact  'word'  ^prefix  suffix$  !negate
#     foo | bar.
#   - <A-h> / <A-i> / <A-r> toggle hidden files, git-ignored files and regex.
#     Hidden files are OFF by default, where telescope passed `--hidden`, so
#     `.github/workflows` needs <A-h> or `-- --hidden`.
#   - <A-p> toggles the preview, the first thing to try if a search still
#     feels slow (it is the maintainer's own diagnostic in snacks
#     discussion #950).
#   - <C-n>/<C-p> navigate and <C-q> sends to the quickfix list. <Tab> is
#     the one remapped key - see `navKeys` below.
#
# Keep `settings.picker` this small. The nixvim `snacks` module types no
# `picker` options, so a misspelled key is silently ignored rather than
# failing the build. `checks.nvim-search` asserts the *resolved* config, so
# add an assertion there for anything set below.
{ pkgs, ... }:

let
  # <Tab>/<S-Tab> walk the candidate list, as they do in fff and telescope.
  # Stock snacks binds them to `select_and_next`/`select_and_prev` instead
  # (`config/defaults.lua:219`), and that is a real bug in practice, not a
  # preference: tabbing down to the 5th match of <leader>ss silently selects
  # matches 1-4 on the way, and <CR> then jumps to the first item of the
  # selection (`actions.lua:47` takes `picker:selected()`, `:136` jumps to
  # `items[1]`). Reproduced headlessly against this config: <C-n> x4 lands on
  # the 5th match, <Tab> x4 lands on the 1st.
  #
  # The direction is the opposite of ./file.nix's fff mapping, and that is not
  # a typo. fff reverse-renders, so its best match is at the BOTTOM and moving
  # up walks toward worse results. snacks' layouts are not reversed, so the best
  # match is at the TOP and moving down walks toward worse results. Same
  # semantics, inverted mapping.
  #
  # Selection is rehomed rather than dropped, onto the <C-Space> that ./file.nix
  # gives fff's `toggle_select`: it is what feeds <C-q> and so `:cfdo`.
  navKeys = modes: {
    "<Tab>".__raw = ''{ "list_down", mode = ${modes} }'';
    "<S-Tab>".__raw = ''{ "list_up", mode = ${modes} }'';
    "<C-Space>".__raw = ''{ "select_and_next", mode = ${modes} }'';
  };
in
{
  keymaps = [
    {
      mode = "n";
      key = "<leader>fd";
      action = "<cmd>lua Snacks.picker.grep()<CR>";
      options.desc = "Grep (live ripgrep)";
    }
    # The duplicate key earns a job: `git grep` over tracked files only.
    {
      mode = "n";
      key = "<leader>fg";
      action = "<cmd>lua Snacks.picker.git_grep()<CR>";
      options.desc = "Grep Git-Tracked Files";
    }
    # `grep_word`'s search is `picker:word()`: the visual selection when there
    # is one, <cword> otherwise (`core/picker.lua:379`). `<cmd>` preserves
    # visual mode, so one keymap covers both.
    {
      mode = [
        "n"
        "x"
      ];
      key = "<leader>fw";
      action = "<cmd>lua Snacks.picker.grep_word()<CR>";
      options.desc = "Grep Word Under Cursor / Selection";
    }
    # Fuzzy over the lines of the open buffer, previewed in the real editor
    # window - its own upstream layout (`config/sources.lua:619`).
    {
      mode = "n";
      key = "<leader>ss";
      action = "<cmd>lua Snacks.picker.lines()<CR>";
      options.desc = "Search in Buffer";
    }
  ];

  plugins.snacks = {
    enable = true;

    # Only `picker` is configured, which keeps the rest of the snacks suite
    # dormant: no dashboard, notifier or explorer comes along.
    settings.picker = {
      # snacks defaults this to `true` and would take over `vim.ui.select`,
      # and with it the LSP code-action dialog. telescope-ui-select keeps it.
      ui_select = false;

      # Live searches stop after this many matches. Upstream's default is
      # 10000, and that is what made live grep lag: every streamed match is
      # parsed, wakes the matcher and is rendered (`core/finder.lua:155`), and
      # a broad query in a large repo produces nearly 10000 of them on every
      # update. Hitting the limit aborts the finder, so ripgrep stops too.
      #
      # 1000 is far more than anyone scrolls through. A query that hits it is
      # too broad to read anyway: type more, or <C-g> to fuzzy-filter what is
      # already there. Live-only, so <leader>ss is unaffected.
      limit_live = 1000;

      win = {
        input.keys = navKeys ''{ "i", "n" }'';
        list.keys = navKeys ''{ "n", "x" }'';
      };
    };
  };

  extraPackages = [
    # snacks' grep hard-requires ripgrep, and `git_grep` needs git on PATH.
    pkgs.ripgrep
    pkgs.git
  ];
}
