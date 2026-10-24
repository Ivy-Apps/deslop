{ pkgs, ... }:

let
  # Inlay hints in VS Code's settings shape. vtsls and tsc read this exact
  # structure, so it is written once and handed to both servers.
  inlayHints = {
    parameterNames = {
      enabled = "all";
      suppressWhenArgumentMatchesName = true;
    };
    parameterTypes.enabled = true;
    variableTypes.enabled = true;
    propertyDeclarationTypes.enabled = true;
    functionLikeReturnTypes.enabled = true;
    enumMemberValues.enabled = true;
  };

  hintedLanguages = {
    typescript.inlayHints = inlayHints;
    javascript.inlayHints = inlayHints;
  };

  # A root belongs to exactly one era, so exactly one server claims it and the
  # other declines by returning without calling `on_dir`. Both hooks are this
  # same function, differing only in the era they answer for.
  rootDirForEra = era: {
    __raw = ''
      function(bufnr, on_dir)
        local root = vim.fs.root(bufnr, { _G.TsLockfiles, { ".git" } }) or vim.fn.getcwd()
        if _G.TsEra(root) ~= "${era}" then
          return
        end
        on_dir(root)
      end
    '';
  };

  # biome owns every filetype these two also offer to format, and conform-nvim
  # runs its CLI on save. Dropping the capability keeps `vim.lsp.buf.format`
  # and any future LSP-driven format path from reformatting against tsserver
  # defaults instead of biome.json.
  yieldFormattingToBiome = {
    __raw = ''
      function(client, _)
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentRangeFormattingProvider = false
      end
    '';
  };
in

{
  autoCmd = [
    {
      event = [
        "BufRead"
        "BufNewFile"
      ];
      pattern = [ "*.mdx" ];
      command = "set filetype=mdx";
    }
  ];

  # The era helpers are defined before all plugin and language Lua, for the
  # same reason `_G.ContextRunners` is: nothing should depend on the order in
  # which the generated config happens to emit them.
  extraConfigLuaPre = ''
    -- TypeScript has two language-service eras, and this config serves both.
    --
    --   classic  TypeScript <= 6. The service is `lib/tsserver.js`, a Node
    --            program speaking the bespoke tsserver protocol. Served by
    --            vtsls, which wraps it.
    --   native   TypeScript >= 7. The service is a Go binary speaking LSP
    --            directly, and `lib/tsserver.js` no longer exists at all.
    --            Served by `tsc --lsp`.
    --
    -- The era is read from the project rather than configured anywhere, and
    -- the probe is the presence of that one file. It tests exactly the
    -- capability the two servers disagree on, so there is no version string to
    -- parse, and a project flips era on its own the moment it upgrades.
    _G.TsLockfiles = { "pnpm-lock.yaml", "package-lock.json", "yarn.lock", "bun.lockb", "bun.lock" }

    _G.TsEra = function(root)
      local classicService = vim.fs.joinpath(root, "node_modules", "typescript", "lib", "tsserver.js")
      if vim.uv.fs_stat(classicService) then
        return "classic"
      end
      return "native"
    end

    -- The native service ships inside the project's own `typescript` package.
    -- Prefer it, so the editor and `pnpm tsc` never disagree about a version;
    -- fall back to the Home Manager one for a root that has not been installed
    -- yet, or a loose file that belongs to no project.
    _G.TsNativeBinary = function(root)
      local vendored = vim.fs.joinpath(root, "node_modules", ".bin", "tsc")
      if vim.fn.executable(vendored) == 1 then
        return vendored
      end
      return "tsc"
    end
  '';

  extraConfigLua = ''
    vim.treesitter.language.register('markdown', 'mdx')

    _G.RegisterContextRunner({
      detect = function(cwd)
        return vim.fn.filereadable(cwd .. "/package.json") == 1
      end,
      run = function(action)
        if action == "test" then
          require("toggleterm").exec("npm run test", 1)
        elseif action == "organize-imports" then
          -- The only source action both eras advertise. The native server
          -- also has `source.removeUnusedImports` and `source.sortImports`,
          -- but vtsls has neither. Note this sorts as well as removing, which
          -- the TSToolsRemoveUnused command it replaces did not.
          vim.lsp.buf.code_action({
            context = { only = { "source.organizeImports" } },
            apply = true,
          })
        end
      end,
    })
  '';

  plugins = {
    treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      typescript
      tsx
      javascript
      html
      css
      markdown
      markdown_inline
    ];

    lsp.servers = {
      biome = {
        enable = true;
        package = null;
      };
      html = {
        enable = true;
        package = null;
      };
      cssls = {
        enable = true;
        package = null;
      };
    };

    nvim-autopairs.settings.ts_config = {
      javascript = [
        "string"
        "template_string"
      ];
    };

    conform-nvim.settings.formatters_by_ft = {
      typescript = [ "biome" ];
      typescriptreact = [ "biome" ];
      javascript = [ "biome" ];
      javascriptreact = [ "biome" ];
      json = [ "biome" ];
      css = [ "biome" ];
      mdx = [ "biome" ];
    };
  };

  # `lsp.servers` rather than `plugins.lsp.servers` above: the latter only has
  # options for server names nvim-lspconfig ships a definition for, and the
  # pinned 2.9.0 has no `lsp/tsc.lua`. The two are the same thing underneath -
  # `plugins.lsp.servers.<name>` is an alias layer writing into `lsp.servers` -
  # so biome, html and cssls are left on the API they were already using.
  lsp.servers = {
    # TypeScript >= 7. Nothing upstream defines this server on the pinned
    # lspconfig, so every field is declared here. `package = null` keeps the
    # binary out of the editor closure: it comes from the project, or from
    # modules/home/languages/typescript.nix.
    tsc = {
      enable = true;
      package = null;
      config = {
        cmd = {
          __raw = ''
            function(dispatchers, config)
              local root = (config or {}).root_dir or vim.fn.getcwd()
              return vim.lsp.rpc.start({ _G.TsNativeBinary(root), "--lsp", "--stdio" }, dispatchers)
            end
          '';
        };
        filetypes = [
          "javascript"
          "javascriptreact"
          "typescript"
          "typescriptreact"
        ];
        root_dir = rootDirForEra "native";
        settings = hintedLanguages;
        on_attach = yieldFormattingToBiome;
      };
    };

    # TypeScript <= 6. nvim-lspconfig's `lsp/vtsls.lua` supplies cmd, filetypes
    # and root markers; only what this repo decides differently is set here.
    vtsls = {
      enable = true;
      package = null;
      config = {
        root_dir = rootDirForEra "classic";
        settings = hintedLanguages // {
          # vtsls vendors its own TypeScript, so a TS 6 project would otherwise
          # be checked by whatever nixpkgs' vtsls happens to bundle. This makes
          # it load the workspace's copy instead, which it finds at the root -
          # exactly where root_dir has already proven a classic service lives.
          # No explicit `typescript.tsdk` is needed: setting one alongside this
          # was verified to change nothing.
          vtsls.autoUseWorkspaceTsdk = true;
        };
        on_attach = yieldFormattingToBiome;
      };
    };
  };
}
