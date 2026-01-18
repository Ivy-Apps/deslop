{ pkgs, haskellPackages }:

{
  colorschemes.catppuccin.enable = true;

  globals = {
    mapleader = " ";
    maplocalleader = " ";
  };

  opts = {
    number = true;
    relativenumber = true;
    shiftwidth = 2;
    expandtab = true;
    smartindent = true;
    breakindent = true;
    ignorecase = true;
    smartcase = true;

    # --- Window/Split Behavior ---
    splitbelow = true;
    splitright = true;
    termguicolors = true;
  };

  clipboard.register = "unnamedplus";

  keymaps = [
    # --- Window Management ---
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
    # Move the actual windows around
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

    # --- Haskell Tools ---
    {
      mode = "n";
      key = "<leader>fm";
      action = "<cmd>lua require('conform').format()<CR>";
      options.desc = "Format (Fourmolu)";
    }
    {
      mode = "n";
      key = "<leader>cl";
      action = "<cmd>!hlint %<CR>";
      options.desc = "Check Lint (Hlint CLI)";
    }
    {
      mode = "n";
      key = "<leader>rr";
      action = "<cmd>lua require('haskell-tools').repl.toggle()<CR>";
      options.desc = "Haskell REPL";
    }
    {
      mode = "n";
      key = "<leader>rq";
      action = "<cmd>lua require('haskell-tools').repl.quit()<CR>";
      options.desc = "Quit GHCi REPL";
    }
    {
      mode = "n";
      key = "<leader>hg";
      action = ''<cmd>lua require("toggleterm").exec("while read -p 'Hoogle> ' q; do hoogle search --color --count=15 \"$q\"; echo; done", 5)<CR>'';
      options = {
        desc = "Hoogle Search (Bottom)";
        silent = true;
      };
    }
    {
      mode = "n";
      key = "<leader>tt";
      action = ''<cmd>lua require("toggleterm").exec("cabal test --test-show-details=direct", 1)<CR>'';
      options.desc = "Run Cabal Tests";
    }
    {
      mode = "n";
      key = "<leader>ca";
      action = "<cmd>lua vim.lsp.buf.code_action()<CR>";
      options.desc = "Code Actions";
    }
    # --- Search & UI ---
    {
      mode = "n";
      key = "<leader>ss";
      action = "<cmd>Telescope current_buffer_fuzzy_find<CR>";
      options.desc = "Search in Buffer";
    }
    {
      mode = "n";
      key = "<leader>nh";
      action = "<cmd>noh<CR>";
      options.desc = "Clear Highlights";
    }
    {
      mode = "n";
      key = "<leader>bk";
      action = "<cmd>bd<CR>";
      options.desc = "Kill Buffer";
    }
    # --- Git ---
    {
      mode = "n";
      key = "<leader>gs";
      action = "<cmd>Neogit<CR>";
      options.desc = "Git Status (Neogit)";
    }
  ];

  plugins = {
    web-devicons.enable = true;
    nvim-tree.enable = true;
    diffview.enable = true;

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

    neogit = {
      enable = true;
      settings.integrations.diffview = true;
    };

    telescope = {
      enable = true;
      keymaps = {
        "<leader>ff" = "find_files";
        "<leader>fg" = "live_grep";
      };
    };

    treesitter = {
      enable = true;
      settings = {
        highlight.enable = true;
        indent.enable = false;
      };
      grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
        haskell
        json
        yaml
        markdown
        nix
        bash
        make
      ];
    };

    haskell-tools = {
      enable = true;
      settings.tools = {
        hover.enable = true;
        log.level = "info";
        repl = {
          handler = "toggleterm";
        };
      };
    };

    conform-nvim = {
      enable = true;
      settings = {
        format_on_save = { timeout_ms = 5000; lsp_fallback = true; };
        formatters_by_ft = {
          haskell = [ "fourmolu" ];
          nix = [ "nixpkgs_fmt" ];
        };
      };
    };

    cmp = {
      enable = true;
      autoEnableSources = true;
      settings = {
        sources = [{ name = "nvim_lsp"; } { name = "path"; } { name = "buffer"; } { name = "luasnip"; }];
        mapping = {
          "<CR>" = "cmp.mapping.confirm({ select = true })";
          "<Tab>" = "cmp.mapping.select_next_item()";
        };
      };
    };
    luasnip.enable = true;
    lsp.enable = true;
    lsp.servers.nil_ls.enable = true;
    gitsigns.enable = true;
    comment.enable = true;
    lualine.enable = true;
  };

  extraPackages = [
    pkgs.ripgrep
    pkgs.fd
    pkgs.nixpkgs-fmt
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.fourmolu
    haskellPackages.hlint
  ];
}

