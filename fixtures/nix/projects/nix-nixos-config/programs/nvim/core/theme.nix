{ lib, config, ... }:

let
  cfg = config.myNixVim;
in
{
  options.myNixVim.style = lib.mkOption {
    type = lib.types.enum [
      "catppuccin_light"
      "catppuccin_dark"
      "tokyonight"
      "auto"
    ];
    default = "catppuccin_dark";
    description = "Select the color theme for NeoVim.";
  };

  config = lib.mkMerge [

    # --- BASE UI & STYLING CONFIGURATION ---
    {
      opts = {
        termguicolors = true;
      };

      plugins = {
        # Core Icons
        web-devicons.enable = true;

        # Next-Gen UI (Command line, search, and notifications)
        notify = {
          enable = true;
          settings = {
            timeout = 1500;
          };
        };
        noice = {
          enable = true;
          settings.presets = {
            bottom_search = true;
            command_palette = true;
            long_message_to_split = true;
            lsp_doc_border = true;
          };
        };

        # Statusline and Tabline
        lualine = {
          enable = true;
          settings.options = {
            component_separators = {
              left = "│";
              right = "│";
            };
            section_separators = {
              left = "";
              right = "";
            };
            theme = "auto";
          };
        };
        bufferline.enable = true;

        # Indentation Guides
        indent-blankline = {
          enable = true;
          settings.scope = {
            enabled = true;
            show_start = true;
          };
        };

        # Keybinding hints popup
        which-key.enable = true;

        # Welcome Dashboard
        alpha = {
          enable = true;
          theme = "dashboard";
        };

        # Nvim-Tree Styling (merges with file-tree.nix settings)
        nvim-tree = {
          enable = true;
          settings = {
            view.width = 30;
            renderer = {
              indent_markers.enable = true;
              icons.glyphs.folder = {
                arrow_closed = "";
                arrow_open = "";
              };
            };
          };
        };

        # Telescope Styling (merges with search.nix settings.defaults)
        telescope = {
          enable = true;
          settings.defaults = {
            prompt_prefix = "   ";
            selection_caret = "  ";
            # Enforce rounded borders for telescope
            borderchars = [
              "─"
              "│"
              "─"
              "│"
              "╭"
              "╮"
              "╯"
              "╰"
            ];
          };
        };

        # Enforce rounded borders for completion windows (merges with auto-complete.nix)
        cmp.settings.window = {
          completion.border = "rounded";
          documentation.border = "rounded";
        };
      };

      # Theme the snacks picker (<leader>fd and the rest of ../search/grep.nix).
      # catppuccin ships an integration for it but defaults it to `enabled =
      # false` (catppuccin/init.lua:116), so it has to be asked for. Worth
      # asking for: it is exactly the work ../search/file.nix has to do by
      # hand for fff, which catppuccin has no integration for at all - fff
      # hardcodes dark-tuned hexes that wash out on latte.
      #
      # Harmless under the tokyonight branch below: nothing is emitted for a
      # colorscheme that is not enabled.
      colorschemes.catppuccin.settings.integrations.snacks.enabled = true;

      keymaps = [
        {
          mode = "n";
          key = "<leader>nd";
          action = "<cmd>NoiceDismiss<CR>";
          options.desc = "Dismiss notifications";
        }
      ];

      # Enforce rounded borders for LSP hovers and diagnostics globally
      extraConfigLua = ''
        local _border = "rounded"
        vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = _border })
        vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = _border })
        vim.diagnostic.config{ float={border=_border} }
      '';
    }

    # --- THEME: CATPPUCCIN LIGHT (Latte) ---
    (lib.mkIf (cfg.style == "catppuccin_light") {
      colorschemes.catppuccin = {
        enable = true;
        settings.flavour = "latte";
      };
      # Latte base colour — avoids black notify background on a light theme
      plugins.notify.settings.backgroundColour = lib.mkForce "#eff1f5";
      # Override notify highlight groups so titles/icons are visible on light bg.
      # Hooked on ColorScheme so they survive any colorscheme reload.
      extraConfigLua = ''
        vim.api.nvim_create_autocmd("ColorScheme", {
          pattern = "*",
          callback = function()
            -- Catppuccin Latte palette colours — dark enough to read on #eff1f5
            vim.api.nvim_set_hl(0, "NotifyERRORTitle",  { fg = "#d20f39" })
            vim.api.nvim_set_hl(0, "NotifyERRORIcon",   { fg = "#d20f39" })
            vim.api.nvim_set_hl(0, "NotifyERRORBorder", { fg = "#d20f39" })
            vim.api.nvim_set_hl(0, "NotifyWARNTitle",   { fg = "#df8e1d" })
            vim.api.nvim_set_hl(0, "NotifyWARNIcon",    { fg = "#df8e1d" })
            vim.api.nvim_set_hl(0, "NotifyWARNBorder",  { fg = "#df8e1d" })
            vim.api.nvim_set_hl(0, "NotifyINFOTitle",   { fg = "#40a02b" })
            vim.api.nvim_set_hl(0, "NotifyINFOIcon",    { fg = "#40a02b" })
            vim.api.nvim_set_hl(0, "NotifyINFOBorder",  { fg = "#40a02b" })
            vim.api.nvim_set_hl(0, "NotifyDEBUGTitle",  { fg = "#7c7f93" })
            vim.api.nvim_set_hl(0, "NotifyDEBUGIcon",   { fg = "#7c7f93" })
            vim.api.nvim_set_hl(0, "NotifyDEBUGBorder", { fg = "#7c7f93" })
            vim.api.nvim_set_hl(0, "NotifyTRACETitle",  { fg = "#8839ef" })
            vim.api.nvim_set_hl(0, "NotifyTRACEIcon",   { fg = "#8839ef" })
            vim.api.nvim_set_hl(0, "NotifyTRACEBorder", { fg = "#8839ef" })
            -- NvimTree selected file: surface1 bg (#bcc0cc) gives clear contrast on latte
            vim.api.nvim_set_hl(0, "NvimTreeCursorLine", { bg = "#bcc0cc" })
          end,
        })
      '';
    })

    # --- THEME: CATPPUCCIN DARK (Mocha) ---
    (lib.mkIf (cfg.style == "catppuccin_dark") {
      colorschemes.catppuccin = {
        enable = true;
        settings.flavour = "mocha";
      };
      plugins.notify.settings.backgroundColour = "#1e1e2e";
    })

    # --- THEME: TOKYONIGHT (Moon) ---
    (lib.mkIf (cfg.style == "tokyonight") {
      plugins.notify.settings.backgroundColour = "#222436";
      colorschemes.tokyonight = {
        enable = true;
        settings = {
          style = "moon";
          transparent = true;
          terminal_colors = true;
          styles = {
            floats = "transparent";
            sidebars = "transparent";
          };
        };
      };
    })

    # --- THEME: AUTO (follows system appearance) ---
    (lib.mkIf (cfg.style == "auto") {
      colorschemes.catppuccin.enable = true;
      extraConfigLua = ''
        local function _is_dark_mode()
          if vim.fn.has("mac") == 1 then
            return vim.trim(vim.fn.system("defaults read -g AppleInterfaceStyle 2>/dev/null")) == "Dark"
          end
          -- Linux / GNOME
          return vim.fn.system("gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null"):find("dark") ~= nil
        end
        local _dark = _is_dark_mode()
        -- catppuccin's setup() REPLACES its options rather than merging into
        -- them (`M.options = vim.tbl_deep_extend("keep", user_conf,
        -- M.default_options)`), so this call discards everything nixvim wrote
        -- through `settings` and anything still wanted has to be repeated.
        require("catppuccin").setup({
          flavour = _dark and "mocha" or "latte",
          integrations = { snacks = { enabled = true } },
        })
        vim.cmd("colorscheme catppuccin")
        -- Match notify background to the resolved theme so it looks correct on light
        require("notify").setup({ background_colour = _dark and "#1e1e2e" or "#eff1f5" })
        -- On light (latte) override notify highlight groups so titles/icons are visible
        if not _dark then
          local function _set_notify_hl()
            vim.api.nvim_set_hl(0, "NotifyERRORTitle",  { fg = "#d20f39" })
            vim.api.nvim_set_hl(0, "NotifyERRORIcon",   { fg = "#d20f39" })
            vim.api.nvim_set_hl(0, "NotifyERRORBorder", { fg = "#d20f39" })
            vim.api.nvim_set_hl(0, "NotifyWARNTitle",   { fg = "#df8e1d" })
            vim.api.nvim_set_hl(0, "NotifyWARNIcon",    { fg = "#df8e1d" })
            vim.api.nvim_set_hl(0, "NotifyWARNBorder",  { fg = "#df8e1d" })
            vim.api.nvim_set_hl(0, "NotifyINFOTitle",   { fg = "#40a02b" })
            vim.api.nvim_set_hl(0, "NotifyINFOIcon",    { fg = "#40a02b" })
            vim.api.nvim_set_hl(0, "NotifyINFOBorder",  { fg = "#40a02b" })
            vim.api.nvim_set_hl(0, "NotifyDEBUGTitle",  { fg = "#7c7f93" })
            vim.api.nvim_set_hl(0, "NotifyDEBUGIcon",   { fg = "#7c7f93" })
            vim.api.nvim_set_hl(0, "NotifyDEBUGBorder", { fg = "#7c7f93" })
            vim.api.nvim_set_hl(0, "NotifyTRACETitle",  { fg = "#8839ef" })
            vim.api.nvim_set_hl(0, "NotifyTRACEIcon",   { fg = "#8839ef" })
            vim.api.nvim_set_hl(0, "NotifyTRACEBorder", { fg = "#8839ef" })
            vim.api.nvim_set_hl(0, "NvimTreeCursorLine", { bg = "#bcc0cc" })
          end
          _set_notify_hl()
          vim.api.nvim_create_autocmd("ColorScheme", { pattern = "*", callback = _set_notify_hl })
        end
      '';
    })

  ];
}
