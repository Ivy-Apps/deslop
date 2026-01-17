{
  description = "Deslop: Haskell Env + Immutable Neovim IDE";

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nixvim, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { config, pkgs, system, ... }:
        let
          ghcVersion = "ghc967";

          haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              deslop = self.callCabal2nix "deslop" ./. { };
            };
          };

          nixvimModule = {
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
              backspace = [ "indent" "eol" "start" ];
              ignorecase = true;
              smartcase = true;
            };

            clipboard.register = "unnamedplus";

            # --- Keymaps ---
            keymaps = [
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
              {
                mode = "n";
                key = "<C-a>";
                action = "ggVG";
                options.desc = "Select All";
              }

              # --- Git ---
              {
                mode = "n";
                key = "<leader>gs";
                action = "<cmd>Neogit<CR>";
                options.desc = "Git Status (Neogit)";
              }
              {
                mode = "n";
                key = "<leader>hp";
                action = "<cmd>Gitsigns preview_hunk<CR>";
                options.desc = "Preview Git Hunk";
              }
            ];

            plugins = {
              web-devicons.enable = true;
              nvim-tree.enable = true;

              diffview.enable = true;

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
                settings.indent.enable = false;
                grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [ haskell json yaml markdown nix bash make ];
              };

              haskell-tools = {
                enable = true;
                settings.tools = {
                  hover.enable = true;
                  log.level = "info";
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
              pkgs.fourmolu
              pkgs.hlint
            ];
          };

          nvim = nixvim.legacyPackages.${system}.makeNixvim nixvimModule;

          ciDeps = [
            haskellPackages.cabal-install
            haskellPackages.hspec-discover
            pkgs.hlint
            pkgs.git
          ];

          localDeps = [
            nvim
            pkgs.just
            haskellPackages.hspec-golden
            haskellPackages.hpack
            haskellPackages.implicit-hie
          ];

        in
        {
          devShells = {
            ci = haskellPackages.shellFor {
              packages = p: [ p.deslop ];
              nativeBuildInputs = ciDeps;
              buildInputs = [ pkgs.zlib pkgs.xz ];
            };

            default = haskellPackages.shellFor {
              packages = p: [ p.deslop ];
              withHoogle = true;

              nativeBuildInputs = ciDeps ++ localDeps;
              buildInputs = [
                pkgs.zlib
                pkgs.xz
                haskellPackages.haskell-language-server
                pkgs.fourmolu
              ];

              shellHook = ''
                echo "🔮 Deslop Dev env initialized."
                echo "--------------------------------------------------------"
                # This will print the exact versions currently in the PATH
                echo "✅ GHC:  $(ghc --version)"
                echo "✅ HLS:  $(haskell-language-server --version | awk '{print $1, $2, $3, $4, $5}')"
                echo "--------------------------------------------------------"
              
                if [ ! -f hie.yaml ]; then
                  echo "   Generating hie.yaml for HLS..."
                  gen-hie > hie.yaml
                fi

                echo "   Run 'vim' to start."
                alias vim="nvim"
              '';
            };
          };
        };
    };
}