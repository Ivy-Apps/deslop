_:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>fm";
      action = "<cmd>lua require('conform').format()<CR>";
      options.desc = "Format";
    }
  ];

  plugins = {
    conform-nvim = {
      enable = true;
      settings = {
        format_on_save = {
          timeout_ms = 2000;
          lsp_fallback = true;
        };
      };
    };
  };

}
