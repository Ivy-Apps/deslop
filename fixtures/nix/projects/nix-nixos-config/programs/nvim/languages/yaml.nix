{ pkgs, ... }:

{
  plugins = {
    treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      yaml
    ];

    lsp.servers.yamlls = {
      enable = true;
      package = null;
    };

    conform-nvim.settings.formatters_by_ft = {
      yaml = [ "prettierd" ];
    };
  };
}
