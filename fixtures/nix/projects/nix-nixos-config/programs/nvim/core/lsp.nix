{ pkgs, ... }:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>e";
      action = "<cmd>lua vim.diagnostic.open_float()<CR>";
      options.desc = "Show line diagnostics";
    }
    {
      mode = "n";
      key = "<leader>gd";
      action = "<cmd>Telescope lsp_definitions<CR>";
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
      key = "<leader>gi";
      action = "<cmd>Telescope lsp_implementations<CR>";
      options.desc = "Go to Implementation (Telescope)";
    }
    {
      mode = [
        "n"
        "v"
      ];
      key = "<leader>ca";
      action = "<cmd>lua vim.lsp.buf.code_action()<CR>";
      options.desc = "Code Actions";
    }
    {
      mode = "n";
      key = "<leader>rn";
      action = "<cmd>lua vim.lsp.buf.rename()<CR>";
      options.desc = "Rename Symbol (LSP)";
    }
  ];

  plugins = {
    treesitter = {
      enable = true;
      settings = {
        highlight.enable = true;
        indent.enable = true;
      };
      grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
        json
        yaml
        markdown
        bash
        dockerfile
        make
      ];
    };

    lsp = {
      enable = true;
    };

    comment.enable = true;
  };
}
