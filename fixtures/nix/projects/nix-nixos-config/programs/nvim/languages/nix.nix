{ pkgs, ... }:

{
  plugins = {
    treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      nix
    ];

    lsp.servers = {
      nil_ls = {
        enable = true;
        settings = {
          formatting.command = [ "nixfmt" ];
          nix.flake.autoArchive = true; # Helps with flake path resolution
        };
      };
    };

    conform-nvim.settings.formatters_by_ft = {
      nix = [ "nixfmt" ];
    };
  };
}
