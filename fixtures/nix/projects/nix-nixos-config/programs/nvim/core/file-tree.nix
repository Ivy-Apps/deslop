_:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>ft";
      action = "<cmd>NvimTreeToggle<CR>";
      options.desc = "File Tree";
    }
    {
      mode = "n";
      key = "<leader>cot";
      # Use __raw to pass a multi-line Lua function directly to the keymap action
      action.__raw = ''
        function()
          local current_buf = vim.api.nvim_get_current_buf()
          local buffers = vim.api.nvim_list_bufs()

          for _, buf in ipairs(buffers) do
            -- Only delete if it's not the current buffer, is valid, and is listed
            -- (nvim-tree is unlisted, so it gets skipped safely)
            if buf ~= current_buf and vim.api.nvim_buf_is_valid(buf) and vim.bo[buf].buflisted then
              vim.api.nvim_buf_delete(buf, { force = false })
            end
          end
          print("Closed all other tabs")
        end
      '';
      options.desc = "Close All Other Tabs";
    }
  ];

  plugins = {
    nvim-tree = {
      enable = true;
      settings = {
        update_focused_file = {
          enable = true;
          update_root = true;
        };
      };
    };
  };
}
