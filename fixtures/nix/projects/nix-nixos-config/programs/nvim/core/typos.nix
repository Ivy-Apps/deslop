{ pkgs, root, ... }:

let
  typos = import "${root}/programs/typos" { inherit pkgs; };
in
{
  plugins.lsp.servers.typos_lsp = {
    enable = true;
    # Baked in for the same reason as nil_ls: typos is not language-specific, so
    # no project dev shell would ever put this binary on PATH.
    package = pkgs.typos-lsp;

    extraOptions.init_options = {
      # Layered on top of whatever `.typos.toml` the workspace provides — the
      # editor equivalent of the CLI's `--config`. See programs/typos.
      config = typos.globalConfigPath;

      # Neovim renders diagnostics as an underline plus a sign and nothing else
      # (`virtual_text` defaults to false), so anything below Warning is easy to
      # miss entirely — a Hint is a faint underline with no message anywhere.
      diagnosticSeverity = "Warning";
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>ad";
      action = "<cmd>lua TyposAddWord()<CR>";
      options.desc = "Add word to typos allowlist";
    }
  ];

  extraConfigLua = ''
    -- Add the typo under the cursor to an allowlist. Two destinations, because
    -- this nixos repo is public: see programs/typos for why.
    local TYPOS_GLOBAL_ALLOWLIST = vim.fn.expand("${typos.globalConfigPath}")

    local function typos_client()
      return vim.lsp.get_clients({ bufnr = 0, name = "typos_lsp" })[1]
    end

    -- The exact token typos flagged, or nil.
    --
    -- Reads the diagnostic rather than <cword> deliberately. `extend-words` is
    -- matched *after* identifiers are split, so with the cursor in `NoiceDismiss`
    -- the flagged token is `Noice`; writing `noicedismiss` would match nothing at
    -- all and fail silently.
    local function flagged_word()
      local client = typos_client()
      if not client then
        return nil
      end

      local pos = vim.api.nvim_win_get_cursor(0)
      local lnum, col = pos[1] - 1, pos[2]

      -- Scope to typos' own namespace so a diagnostic from another server on the
      -- same line is never picked up by mistake.
      local opts = { lnum = lnum }
      local ok, ns = pcall(vim.lsp.diagnostic.get_namespace, client.id)
      if ok then
        opts.namespace = ns
      end

      for _, d in ipairs(vim.diagnostic.get(0, opts)) do
        if d.lnum == lnum and col >= d.col and col < d.end_col then
          return vim.api.nvim_buf_get_text(0, d.lnum, d.col, d.end_lnum, d.end_col, {})[1]
        end
      end
      return nil
    end

    -- Insert `word = "word"` under [default.extend-words], creating the table (or
    -- the whole file) when absent. Returns false if the word is already listed.
    local function allow_word(path, word)
      local lines = vim.fn.filereadable(path) == 1 and vim.fn.readfile(path) or {}
      local pattern = "^%s*" .. vim.pesc(word) .. "%s*="

      for _, line in ipairs(lines) do
        if line:match(pattern) then
          return false
        end
      end

      local header
      for i, line in ipairs(lines) do
        if line:match("^%s*%[default%.extend%-words%]%s*$") then
          header = i
          break
        end
      end

      local entry = string.format('%s = "%s"', word, word)
      if header then
        table.insert(lines, header + 1, entry)
      else
        if #lines > 0 and lines[#lines] ~= "" then
          table.insert(lines, "")
        end
        table.insert(lines, "[default.extend-words]")
        table.insert(lines, entry)
      end

      vim.fn.mkdir(vim.fn.fnamemodify(path, ":h"), "p")
      vim.fn.writefile(lines, path)
      return true
    end

    -- typos-lsp does not watch its config files: "Restart the server after
    -- changing either the workspace config file or the explicit custom config
    -- file for the new changes to take effect." Without this the word appears to
    -- have no effect at all.
    local function restart_typos()
      vim.lsp.enable("typos_lsp", false)
      vim.schedule(function()
        vim.lsp.enable("typos_lsp", true)
      end)
    end

    local function project_allowlist()
      local client = typos_client()
      local root = (client and client.root_dir) or vim.fn.getcwd()
      return root .. "/.typos.toml"
    end

    function TyposAddWord()
      local word = flagged_word()
      if not word then
        vim.notify("typos: no typo under the cursor", vim.log.levels.WARN)
        return
      end

      -- Lowercase: matching is case-insensitive, so one entry covers every casing.
      word = word:lower()

      local destinations = {
        { label = "Global (private, never committed)", path = TYPOS_GLOBAL_ALLOWLIST },
        { label = "This project (.typos.toml, committed)", path = project_allowlist() },
      }

      vim.ui.select(destinations, {
        prompt = string.format("Allow '%s' in:", word),
        format_item = function(item)
          return item.label
        end,
      }, function(choice)
        if not choice then
          return
        end

        local ok, added = pcall(allow_word, choice.path, word)
        if not ok then
          vim.notify("typos: " .. tostring(added), vim.log.levels.ERROR)
          return
        end

        if not added then
          vim.notify(string.format("typos: '%s' is already allowed in %s", word, choice.path))
          return
        end

        vim.notify(string.format("typos: allowed '%s' in %s", word, choice.path))
        restart_typos()
      end)
    end
  '';
}
