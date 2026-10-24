_:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>gs";
      action = "<cmd>Neogit<CR>";
      options.desc = "Git Status (Neogit)";
    }
  ];

  plugins = {
    diffview.enable = true;
    neogit = {
      enable = true;
      settings.integrations.diffview = true;
    };
    gitsigns.enable = true;
  };
}
