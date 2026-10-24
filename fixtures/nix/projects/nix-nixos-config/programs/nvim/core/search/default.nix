{ lib, ... }:

let
  shared = import ./shared.nix { inherit lib; };
in
{
  imports = [
    ./file.nix
    ./grep.nix
  ];

  keymaps = [
    # Reopen the previous content search with its query, cursor position and
    # multi-selection intact (`snacks/picker/resume.lua:28`). snacks only:
    # fff has no resume at all, so <leader>ff always starts fresh, and
    # telescope's own resume is deliberately no longer bound - the searches
    # worth resuming are the long grep queries, not the one-shot <leader>fa
    # and <leader>fe finders.
    {
      mode = "n";
      key = "<leader>fl";
      action = "<cmd>lua Snacks.picker.resume()<CR>";
      options.desc = "Resume Last Search";
    }
    {
      mode = "n";
      key = "<leader>nh";
      action = "<cmd>noh<CR>";
      options.desc = "Clear Highlights";
    }
  ];

  # Engine-level telescope config: the parts that apply to every telescope
  # picker and so belong to neither ./file.nix nor ./grep.nix. snacks' own
  # engine config lives in ./grep.nix, which is its only consumer.
  #
  # Telescope still owns <leader>fa, <leader>fe and <leader>fr here, plus the
  # LSP pickers in ../lsp.nix, neoclip in ../clipboard.nix, and the
  # `vim.ui.select` provider behind every code-action dialog.
  plugins.telescope = {
    enable = true;

    # Derived from the one `excludeDirs` list in ./shared.nix, so this can
    # never drift from the `fd --exclude` flags the file pickers use.
    settings.defaults.file_ignore_patterns = shared.ignorePatterns;

    extensions = {
      ui-select.enable = true;
      # Compiled sorter: faster fuzzy matching on large result sets and
      # unlocks fzf query syntax in every prompt:
      #   'exact   ^prefix   suffix$   !negate   foo | bar
      fzf-native = {
        enable = true;
        settings = {
          fuzzy = true;
          override_generic_sorter = true;
          override_file_sorter = true;
          case_mode = "smart_case";
        };
      };
    };
  };
}
