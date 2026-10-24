{ pkgs, themeConfig, ... }:
let
  ghosttyTheme =
    {
      dark = "TokyoNight";
      light = "TokyoNight Day";
      auto = "light:TokyoNight Day,dark:TokyoNight";
    }
    .${themeConfig};
in
{
  # fontconfig is Linux-only; macOS locates fonts via ~/Library/Fonts automatically
  fonts.fontconfig.enable = pkgs.stdenv.isLinux;

  programs = {
    ghostty = {
      enable = true;
      package = if pkgs.stdenv.isDarwin then null else pkgs.ghostty;
      enableZshIntegration = true;

      settings = {
        theme = ghosttyTheme;
        font-family = "JetBrainsMono Nerd Font";
        font-size = 20;

        window-decoration = true;
        # Maps Option to Alt so Neovim keybinds work on macOS; ignored on Linux
        macos-option-as-alt = true;
        macos-window-shadow = true;

        scrollback-limit = 10000;
        copy-on-select = true;
        mouse-hide-while-typing = true;

        keybind = [
          "ctrl+t=new_tab"
          "ctrl+w=close_surface"
          "ctrl+tab=next_tab"
          "ctrl+shift+tab=previous_tab"
          "ctrl+shift+enter=new_split:right"
          "ctrl+shift+down=new_split:down"
          "ctrl+left=goto_split:left"
          "ctrl+right=goto_split:right"
          "super+n=new_window"
          "ctrl+shift+n=new_window"
        ];
      };
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;

      shellAliases = {
        # General
        nv = "nvim";
        ls = "eza --icons";
        ll = "eza -alF --icons";

        # Git — core
        gs = "git status";
        gd = "git diff";
        gds = "git diff --staged";

        # Git — staging & committing
        gad = "git add";
        gcm = "git commit -m";

        # Git — branches
        gb = "git branch";
        gbd = "git branch -D";

        # Legacy checkout (kept for muscle memory / detached HEAD workflows)
        gco = "git checkout";
        gcob = "git checkout -b";

        # Git — remote
        gp = "git pull";
        gpu = "git push";
        gpuf = "git push --force-with-lease";

        # Git — log
        gl = "git log --graph --decorate";
        gll = "git log --graph --decorate --stat";

        # Git — reset
        grh = "git reset --hard";
        grs = "git reset --soft";
      };

      initContent = ''
        # Jump to a dir with zoxide, then open Neovim there
        # Usage: nvz <query>
        nvz() {
          z "$@" && nvim .
        }

        # Interactive zoxide jump (fzf picker), then open Neovim there
        # Usage: nvzi [query]
        nvzi() {
          zi "$@" && nvim .
        }

        # Delete all local branches except main and any branches passed as args
        # Usage: gbd_all [branch1 branch2 ...]
        gbd_all() {
          local keep=("main" "$@")
          git branch | grep -v '^\*' | while read -r branch; do
            local skip=0
            for k in "''${keep[@]}"; do
              [[ "$branch" == "$k" ]] && skip=1 && break
            done
            [[ $skip -eq 0 ]] && git branch -D "$branch"
          done
        }
      '';
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };

    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        add_newline = false;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[✗](bold red)";
        };
      };
    };
  };

  home.packages = with pkgs; [
    eza
    ripgrep
    fd
    jq
    tree
    nerd-fonts.jetbrains-mono
  ];
}
