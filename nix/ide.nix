{ pkgs, hpkgs }:

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
    require("telescope").load_extension("hoogle")
    require("telescope").load_extension("live_grep_args")
    
    local cmp = require("cmp")
    local cmp_autopairs = require("nvim-autopairs.completion.cmp")
    cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

    _G.HlsRestart = function()
      vim.lsp.stop_client(vim.lsp.get_active_clients({ name = 'haskell-tools.nvim' }))
      vim.cmd('edit') -- Reload buffer to re-trigger attachment
      vim.notify("♻️  HLS Restarted", vim.log.levels.INFO)
    end
  '';

  keymaps = import ./keymaps.nix;

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
      # package = hpkgs.haskell-language-server;
      settings = {
        hls = {
          enable = true;
          command = "${hpkgs.haskell-language-server}/bin/haskell-language-server";
          onAttach = ''
            function(client, bufnr)
              local opts = { noremap = true, silent = true, buffer = bufnr }
              -- Force-bind K to LSP hover if it's being overridden
              vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
              -- Useful for Haskell: Go to definition
              vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
              -- Codelenes (like type signatures above functions)
              vim.keymap.set('n', '<leader>cl', vim.lsp.codelens.run, opts)
            end
          '';
          settings = {
            haskell = {
              formattingProvider = "fourmolu";
              plugin = {
                importLens = { globalOn = true; };
                alternateNumberFormat = { globalOn = true; };
                ghcide-type-lenses = { globalOn = true; };
                ghcide-code-actions-fill-hole = { globalOn = true; };
                ghcide-code-actions-imports-exports = { globalOn = true; };
              };
            };
          };
        };
        tools = {
          repl = {
            handler = "toggleterm";
          };
        };
      };
    };
    lsp = {
      enable = true;
      capabilities = "require('cmp_nvim_lsp').default_capabilities()";
      servers = {
        nil_ls.enable = true;
        hls.enable = false;
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
    nvim-autopairs = {
      enable = true;
      settings = {
        # Use treesitter to check for pair status
        # Critical for Haskell to distinguish between string literals and 
        # primed variables (e.g. let x' = 5)
        check_ts = true;
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
    hpkgs.fourmolu
    hpkgs.hlint
    hpkgs.hoogle
    hpkgs.cabal-install
    hpkgs.haskell-language-server
  ];
}
