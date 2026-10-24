_:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>h";
      action = "<C-w>h";
      options.desc = "Focus Left";
    }
    {
      mode = "n";
      key = "<leader>l";
      action = "<C-w>l";
      options.desc = "Focus Right";
    }
    {
      mode = "n";
      key = "<leader>j";
      action = "<C-w>j";
      options.desc = "Focus Down";
    }
    {
      mode = "n";
      key = "<leader>k";
      action = "<C-w>k";
      options.desc = "Focus Up";
    }
    {
      mode = "n";
      key = "<leader>H";
      action = "<C-w>H";
      options.desc = "Move Window Left";
    }
    {
      mode = "n";
      key = "<leader>L";
      action = "<C-w>L";
      options.desc = "Move Window Right";
    }
    {
      mode = "n";
      key = "<leader>J";
      action = "<C-w>J";
      options.desc = "Move Window Down";
    }
    {
      mode = "n";
      key = "<leader>K";
      action = "<C-w>K";
      options.desc = "Move Window Up";
    }
    {
      mode = "n";
      key = "<leader>wp";
      action = "<C-w>p";
      options.desc = "Jump to Previous Window";
    }
    {
      mode = "n";
      key = "<leader>>";
      action = "<cmd>vertical resize +15<CR>";
      options = {
        desc = "Make window wider horizontally";
      };
    }
    {
      mode = "n";
      key = "<leader><";
      action = "<cmd>vertical resize -15<CR>";
      options = {
        desc = "Make window narrower horizontally";
      };
    }
  ];

  opts = {
    # --- Window/Split Behavior ---
    splitbelow = true;
    splitright = true;
  };

  plugins = {
    toggleterm = {
      enable = true;
      settings = {
        direction = "horizontal";
        size = ''
          function(term)
            return vim.o.lines * 0.3
          end
        '';
        open_mapping = "[[<c-t>]]";
        hide_numbers = true;
        shade_terminals = true;
        start_in_insert = true;
        terminal_mappings = true;
        persist_mode = true;
        insert_mappings = true;
      };
    };
  };
}
