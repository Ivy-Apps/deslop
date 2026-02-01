{ pkgs, ... }:

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

  extraConfigLua = ''
    -- Nuclear HLS Reset (Fixed Async)
    _G.NuclearHLS = function()
      -- 1. Notify and Stop
      vim.notify("☢️  Initiating HLS Nuclear Restart...", vim.log.levels.WARN)
      
      -- Stop all clients named "hls"
      local clients = vim.lsp.get_clients({ name = "hls" })
      for _, client in ipairs(clients) do
        client.stop()
      end

      -- 2. Wait 1.5s for clean shutdown, then Start
      vim.defer_fn(function()
        vim.cmd("LspStart hls")
        
        -- 3. Wait 1s for attachment, then Verify & Notify
        vim.defer_fn(function()
          local new_clients = vim.lsp.get_clients({ name = "hls" })
          if #new_clients > 0 then
             vim.notify("✅ HLS successfully restarted and attached!", vim.log.levels.INFO)
          else
             vim.notify("❌ HLS failed to attach. Check :LspInfo", vim.log.levels.ERROR)
          end
        end, 1000)
      end, 1500)
    end

    -- Load the manual extension for Hoogle
    require("telescope").load_extension("hoogle")
    -- Load live_grep_args extension
    require("telescope").load_extension("live_grep_args")
  '';

  keymaps = [
    # --- Advanced Search ---
    {
      mode = "n";
      key = "<leader>fg";
      # Using the extension allows passing args to ripgrep (e.g. "String" -t haskell)
      action = "<cmd>lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>";
      options.desc = "Live Grep (Args)";
    }

    # --- Window Management ---
    { mode = "n"; key = "<leader>h"; action = "<C-w>h"; options.desc = "Focus Left"; }
    { mode = "n"; key = "<leader>l"; action = "<C-w>l"; options.desc = "Focus Right"; }
    { mode = "n"; key = "<leader>j"; action = "<C-w>j"; options.desc = "Focus Down"; }
    { mode = "n"; key = "<leader>k"; action = "<C-w>k"; options.desc = "Focus Up"; }

    { mode = "n"; key = "<leader>H"; action = "<C-w>H"; options.desc = "Move Window Left"; }
    { mode = "n"; key = "<leader>L"; action = "<C-w>L"; options.desc = "Move Window Right"; }
    { mode = "n"; key = "<leader>J"; action = "<C-w>J"; options.desc = "Move Window Down"; }
    { mode = "n"; key = "<leader>K"; action = "<C-w>K"; options.desc = "Move Window Up"; }

    # --- Haskell Tools & LSP ---
    {
      mode = "n";
      key = "<leader>e";
      action = "<cmd>lua vim.diagnostic.open_float()<CR>";
      options.desc = "Show line diagnostics";
    }
    {
      mode = "n";
      key = "<leader>fm";
      action = "<cmd>lua require('conform').format()<CR>";
      options.desc = "Format (Fourmolu)";
    }
    {
      mode = "n";
      key = "<leader>gd";
      action = "<cmd>lua vim.lsp.buf.definition()<CR>";
      options.desc = "Go to Definition";
    }
    {
      mode = "n";
      key = "<leader>gr";
      action = "<cmd>Telescope lsp_references<CR>";
      options.desc = "Find References (Telescope)";
    }
    {
      mode = "n";
      key = "<leader>rn";
      action = "<cmd>lua vim.lsp.buf.rename()<CR>";
      options.desc = "Rename Symbol (LSP)";
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
    # Fixed duplicate keymap. Now 'hg' is Telescope (Interactive), 'hs' is Search (Prompt)
    {
      mode = "n";
      key = "<leader>hg";
      action = "<cmd>Telescope hoogle<CR>";
      options.desc = "Hoogle (Live)";
    }
    {
      mode = "n";
      key = "<leader>hs";
      action = "<cmd>lua _G.HoogleSearch()<CR>";
      options.desc = "Hoogle (Prompt)";
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
    {
      mode = "n";
      key = "<leader>lx";
      action = "<cmd>lua _G.NuclearHLS()<CR>";
      options.desc = "Nuclear HLS Restart";
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

    # --- Terminal mode ---
    {
      mode = "t";
      key = "<Esc>";
      action = "<C-\\><C-n>";
      options.desc = "Exit terminal mode";
    }
  ];

  plugins = {
    web-devicons.enable = true;

    nvim-tree = {
      enable = true;
      settings = {
        update_focused_file = {
          enable = true;
          update_root = true;
        };
      };
    };

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
      extensions.live-grep-args.enable = true;
      keymaps = {
        "<leader>ff" = "find_files";
        # Note: <leader>fg is handled manually in keymaps to access the extension
      };
    };

    treesitter = {
      enable = true;
      settings = {
        highlight.enable = true;
        # Note: Haskell Treesitter indent is often buggy, handle with care. 
        # Formatter (fourmolu) is preferred.
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
      settings = {
        hls = {
          cmd = [
            "haskell-language-server-wrapper"
            "--lsp"
            "+RTS"
            "-A128M" # Large allocation area (faster re-checking)
            "-H2G" # Pre-allocate 2GB heap (stops early GC thrashing)
            "-I0" # Disable Idle GC (prevents lag when you stop typing)
            "-RTS"
          ];
        };

        tools = {
          hover.enable = true;
          log.level = "info";
          repl = {
            handler = "toggleterm";
          };
        };
      };
    };

    conform-nvim = {
      enable = true;
      settings = {
        # format_on_save = { timeout_ms = 5000; lsp_fallback = true; };
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
          # Cycle with Tab
          "<Tab>" = "cmp.mapping.select_next_item()";
          "<S-Tab>" = "cmp.mapping.select_prev_item()";

          # IntelliJ-style: Enter replaces the existing text
          "<CR>" = ''
            cmp.mapping.confirm({
              behavior = cmp.ConfirmBehavior.Replace,
              select = true,
            })
          '';

          # Shift+Enter to just insert (without replacing)
          "<S-CR>" = ''
            cmp.mapping.confirm({
              behavior = cmp.ConfirmBehavior.Insert,
              select = true,
            })
          '';
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

  extraPlugins = [
    pkgs.vimPlugins.telescope_hoogle
  ];

  extraPackages = [
    pkgs.ripgrep
    pkgs.fd
    pkgs.nixpkgs-fmt
    pkgs.xdg-utils
  ];
}
