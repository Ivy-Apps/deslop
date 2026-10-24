{ pkgs, ... }:

{
  extraConfigLua = ''
    _G.RegisterContextRunner({
      detect = function(cwd)
        return vim.fn.filereadable(cwd .. "/mix.exs") == 1
      end,
      run = function(action)
        if action == "test" then
          require("toggleterm").exec("mix test", 1)
        end
      end,
    })
  '';

  plugins = {
    treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      elixir
      eex
      heex
    ];

    lsp.servers.elixirls = {
      enable = true;
      package = null;
    };

    conform-nvim.settings.formatters_by_ft = {
      elixir = [ "mix" ];
      eelixir = [ "mix" ];
      heex = [ "mix" ];
    };
  };
}
