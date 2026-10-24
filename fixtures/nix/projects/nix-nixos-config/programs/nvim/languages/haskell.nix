{ pkgs, ... }:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>rr";
      action = ''<cmd>lua require("toggleterm").exec("cabal repl", 1)<CR>'';
      options.desc = "Haskell REPL";
    }
    {
      mode = "n";
      key = "<leader>lx";
      action = "<cmd>lua _G.HlsRestart()<CR>";
      options.desc = "Restart Haskell LSP (HLS)";
    }
    {
      mode = "n";
      key = "<leader>tf";
      action = "<cmd>lua _G.HaskellRunCurrentSpec()<CR>";
      options.desc = "Test current file (hspec match)";
    }
    {
      mode = "n";
      key = "<leader>cl";
      # Runs the code lens on the current line: eval `-- >>> expr` doctests,
      # insert inferred type signatures, etc. Uses <cmd>...<CR> so it actually
      # calls the Lua fn instead of being typed as keystrokes.
      action = "<cmd>lua vim.lsp.codelens.run()<CR>";
      options.desc = "Run CodeLens (eval / type sig)";
    }
  ];

  extraConfigLua = ''
    _G.HaskellRemoveUnusedImports = function()
      vim.lsp.buf.code_action({
        filter = function(action)
          return action.title == "Remove all redundant imports"
        end,
        apply = true,
      })
    end

    _G.HlsRestart = function()
      vim.lsp.stop_client(vim.lsp.get_active_clients({ name = 'hls' }))
      vim.cmd('edit')
      vim.notify("♻️  HLS Restarted", vim.log.levels.INFO)
    end

    -- Run only the current spec file's tests via hspec's --match filter.
    -- Derives the leaf module name (dropping a trailing Spec/Test suffix),
    -- which is what hspec-discover uses as the top-level describe label, and
    -- passes it as an infix pattern. Reuses the test terminal (1).
    _G.HaskellRunCurrentSpec = function()
      local mod = nil
      for _, line in ipairs(vim.api.nvim_buf_get_lines(0, 0, 300, false)) do
        local m = line:match("^module%s+([%w%.]+)")
        if m then mod = m; break end
      end
      if not mod then
        mod = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":t:r")
      end
      mod = mod:gsub("Spec$", ""):gsub("Test$", "")
      local leaf = mod:match("([%w]+)$")
      if not leaf or leaf == "" then
        vim.notify("Could not derive a spec name from this file", vim.log.levels.WARN)
        return
      end
      require("toggleterm").exec("cabal test --test-options='--match \"" .. leaf .. "\"'", 1)
    end

    _G.RegisterContextRunner({
      detect = function(cwd)
        return vim.fn.glob(cwd .. "/*.cabal") ~= ""
          or vim.fn.filereadable(cwd .. "/cabal.project") == 1
      end,
      run = function(action)
        if action == "test" then
          require("toggleterm").exec("cabal test --test-show-details=direct", 1)
        elseif action == "organize-imports" then
          _G.HaskellRemoveUnusedImports()
        end
      end,
    })
  '';

  autoCmd = [
    {
      event = [
        "BufEnter"
        "CursorHold"
        "InsertLeave"
      ];
      pattern = [ "*.hs" ];
      callback = {
        __raw = "function() vim.lsp.codelens.refresh() end";
      };
    }
    {
      # Haskell indentation: nvim-treesitter's Haskell indent queries are
      # incomplete and return 0 for most positions, so pressing <CR> inside a
      # do-block dumps the cursor to column 0. Swap to plain autoindent (copy
      # the previous line's indent), which is the predictable choice for a
      # whitespace-significant language. Scoped to Haskell buffers only; every
      # other filetype keeps its global treesitter indent + smartindent.
      event = [ "FileType" ];
      pattern = [ "haskell" ];
      callback = {
        __raw = ''
          function(args)
            vim.bo[args.buf].smartindent = false
            vim.bo[args.buf].autoindent = true
            -- 4-space indent to match fourmolu (global default is 2). The
            -- where keyword lands at 2 spaces in fourmolu style; format-on-save
            -- normalizes that, so typing at multiples of 4 stays comfortable.
            vim.bo[args.buf].shiftwidth = 4
            vim.bo[args.buf].softtabstop = 4
            vim.bo[args.buf].tabstop = 4
            -- Clear treesitter's indentexpr *after* it attaches on FileType.
            vim.schedule(function()
              if vim.api.nvim_buf_is_valid(args.buf) then
                vim.bo[args.buf].indentexpr = ""
              end
            end)
          end
        '';
      };
    }
  ];

  plugins = {
    treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      haskell
    ];

    lsp = {
      capabilities = "require('cmp_nvim_lsp').default_capabilities()";

      servers.hls = {
        enable = true;
        package = null;
        installGhc = false;
        settings = {
          haskell = {
            formattingProvider = "fourmolu";
            # Plugin IDs verified against this pin via `haskell-language-server
            # --list-plugins`. Unknown IDs are silently ignored by HLS, so keep
            # these exact. Note: hlint and refineImports are NOT built into this
            # HLS, so they are intentionally absent.
            plugin = {
              # Imports
              importLens = {
                globalOn = true;
              }; # make imports explicit (code action)
              "ghcide-extend-import-action" = {
                globalOn = true;
              }; # add a symbol to an existing import list
              qualifyImportedNames = {
                globalOn = true;
              }; # qualify imported names

              # Code actions / refactors
              class = {
                globalOn = true;
              }; # fill in typeclass method stubs
              gadt = {
                globalOn = true;
              }; # convert a datatype to GADT syntax
              "explicit-fields" = {
                globalOn = true;
              }; # expand record wildcards to explicit fields
              "overloaded-record-dot" = {
                globalOn = true;
              }; # rewrite selectors to record-dot syntax
              moduleName = {
                globalOn = true;
              }; # fix a wrong module name
              changeTypeSignature = {
                globalOn = true;
              }; # fix a wrong type signature
              "ghcide-code-actions-type-signatures" = {
                globalOn = true;
              }; # add missing type signatures
              "ghcide-code-actions-fill-holes" = {
                globalOn = true;
              }; # fill typed holes
              "ghcide-code-actions-imports-exports" = {
                globalOn = true;
              }; # import/export quick fixes

              # Pragmas
              "pragmas-completion" = {
                globalOn = true;
              }; # complete {-# LANGUAGE ... #-}
              "pragmas-suggest" = {
                globalOn = true;
              }; # add a missing LANGUAGE pragma
              "pragmas-disable" = {
                globalOn = true;
              }; # disable a warning via pragma

              # Lenses / hovers
              "ghcide-type-lenses" = {
                globalOn = true;
              }; # inferred type signatures as code lenses
              eval = {
                globalOn = true;
              }; # evaluate `-- >>> expr` doctest comments inline
              alternateNumberFormat = {
                globalOn = true;
              }; # toggle numeric literal formats
              "explicit-fixity" = {
                globalOn = true;
              }; # show operator fixity in hovers

              # Rename (crossModule = rename edits other modules too, not just this file)
              rename = {
                globalOn = true;
                config.crossModule = true;
              };

              # Static analysis / highlighting
              stan = {
                globalOn = true;
              }; # stan static-analysis diagnostics
              semanticTokens = {
                globalOn = true;
              }; # HLS semantic highlighting layered over treesitter
            };
          };
        };
      };
    };

    conform-nvim.settings.formatters_by_ft = {
      haskell = [ "fourmolu" ];
      cabal = [ "cabal_fmt" ];
    };
  };
}
