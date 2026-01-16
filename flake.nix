{
  description = "Deslop: Haskell Env + Immutable Neovim IDE";

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
        # --- 1. Haskell Configuration ---
        ghcVersion = "ghc910";
        
        haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
          overrides = self: super: {
            deslop = self.callCabal2nix "deslop" ./. {};
          };
        };

        # --- 2. Neovim Configuration ---
        nixvimModule = {
          enable = true;
          colorschemes.catppuccin.enable = true;

          globals = {
            mapleader = " ";
            maplocalleader = " ";
          };
          
          opts = { 
            number = true; relativenumber = true; 
            shiftwidth = 2; expandtab = true; 
            smartindent = true; breakindent = true;
            backspace = [ "indent" "eol" "start" ];
          };
          
          clipboard.register = "unnamedplus";
          
          # --- Keymaps ---
          keymaps = [
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
             # NEW: Toggle Haskell Repl for current buffer
             {
                mode = "n";
                key = "<leader>rr";
                action = "<cmd>HaskellRep<CR>";
                options.desc = "Haskell REPL";
             }
          ];

          plugins = {
            nvim-tree.enable = true;
            telescope = { enable = true; keymaps = { "<leader>ff" = "find_files"; "<leader>fg" = "live_grep"; }; };
            
            treesitter = {
              enable = true;
              settings.indent.enable = false; 
              grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [ haskell cabal json yaml markdown nix bash make ];
            };

            # Haskell-tools provides the LSP config, so we don't need 'lsp.servers.hls'
            haskell-tools = { 
              enable = true; 
              tools = {
                hover.enable = true; 
                # This ensures the plugin doesn't try to download its own HLS binary
                # and uses the one we provide in extraPackages/PATH
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
            
            # The Critical Trio
            haskellPackages.haskell-language-server
            haskellPackages.hoogle
            
            # The Stable Tools
            pkgs.fourmolu 
            pkgs.hlint
          ];
        };
        
        nvim = nixvim.legacyPackages.${system}.makeNixvim nixvimModule;

        # --- 3. Shell Definitions ---
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
          # NEW: Helper to generate hie.yaml for IDE support
          haskellPackages.implicit-hie
        ];

      in {
        devShells = {
          ci = haskellPackages.shellFor {
            packages = p: [ p.deslop ];
            nativeBuildInputs = ciDeps;
            buildInputs = [ pkgs.zlib ];
          };

          default = haskellPackages.shellFor {
            packages = p: [ p.deslop ];
            withHoogle = true;

            nativeBuildInputs = ciDeps ++ localDeps;
            buildInputs = [ 
                pkgs.zlib 
                haskellPackages.haskell-language-server
                pkgs.fourmolu
            ];

            shellHook = ''
              echo "🔮 Deslop IDE (GHC 9.10)"
              
              # Auto-generate hie.yaml if missing. 
              # This tells HLS strictly how to interpret the project.
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