_:

{
  extraConfigLua = ''
    -- Integration between nvim-autopairs and nvim-cmp
    local cmp_autopairs = require('nvim-autopairs.completion.cmp')
    local cmp = require('cmp')
    cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
  '';

  plugins = {
    cmp = {
      enable = true;
      autoEnableSources = true;
      settings = {
        snippet = {
          expand = ''
            function(args)
              require('luasnip').lsp_expand(args.body)
            end
          '';
        };
        sources = [
          { name = "nvim_lsp"; }
          { name = "path"; }
          { name = "buffer"; }
          { name = "luasnip"; }
        ];
        mapping = {
          # Use Tab to cycle through suggestions
          "<Tab>" = "cmp.mapping.select_next_item()";
          "<S-Tab>" = "cmp.mapping.select_prev_item()";

          # IntelliJ-style: Enter replaces the existing text
          "<CR>" = ''
            cmp.mapping.confirm({
              behavior = cmp.ConfirmBehavior.Replace,
              select = false,
            })
          '';
        };
      };
    };

    nvim-autopairs = {
      enable = true;
      settings = {
        check_ts = true; # Use treesitter to check for a pair
        ts_config = {
          lua = [
            "string"
            "source"
          ];
        };
        # This allows it to work with nvim-cmp
        fast_wrap = {
          enable = true;
        };
      };
    };

    luasnip.enable = true;

    # Surround text objects (ys, cs, ds)
    nvim-surround.enable = true;
  };
}
