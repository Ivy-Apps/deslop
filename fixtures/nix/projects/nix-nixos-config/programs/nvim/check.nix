# Build-time tests for the Neovim config, wired into `nix flake check` (and
# therefore `just check` in CI) as `checks.nvim-typescript` and
# `checks.nvim-search`. Returns an attrset so both share the one real editor
# derivation rather than building it twice.
#
# What prompts a check here: a config that Nix is perfectly happy with but
# that is wrong at runtime. Two shapes of that have bitten this config.
#
# TypeScript 7 removed lib/tsserver.js, so the server that had been serving
# every project could no longer start - the failure only appeared as a Lua
# error in a buffer.
#
# And `plugins.snacks.settings` and `plugins.fff.settings` are freeform in
# nixvim: no `settingsOptions` for the keys this config actually sets, so a
# misspelled one is dropped silently and the plugin runs on its defaults with
# nothing to show that it happened.
#
# And a picker key that does the wrong thing only shows up when the keys are
# actually pressed, so `checks.nvim-search` presses them.
#
# No language server is started by either check: nothing is `:edit`ed, no
# filetype is set, and the `root_dir` hooks are called directly on named
# buffers. That keeps them hermetic and fast, and is also their limit - the
# TypeScript one proves which server claims a root, not that the server then
# starts.
{
  pkgs,
  inputs,
  root,
}:

let
  inherit (pkgs.stdenv.hostPlatform) system;

  nvim = inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
    inherit pkgs;
    module = import "${root}/programs/nvim";
    extraSpecialArgs = { inherit root; };
  };

  # Runs `body` in a real headless Neovim and fails the build on any
  # assertion it recorded.
  #
  # A Lua error inside `-c luafile` aborts the chunk but leaves Neovim's exit
  # code at 0, so no check here can rely on that alone: `finish()` writes a
  # sentinel as its last act and the builder fails when it is missing.
  # Reaching it is the only proof the assertions all ran.
  #
  # `body` must call `finish()` exactly once. That is what lets a check drive
  # the editor with real keystrokes: feedkeys only run once the script has
  # returned to the event loop, so a body that needs them schedules its
  # assertions and calls `finish()` from the last step, rather than running
  # top to bottom and being quit before anything happened. A body that never
  # finishes is failed by the timeout instead of hanging the build.
  mkCheck =
    {
      name,
      body,
      fixtures ? "",
    }:
    let
      script = pkgs.writeText "nvim-${name}-assertions.lua" ''
        local failures = {}

        local function expect(what, actual, wanted)
          if not vim.deep_equal(actual, wanted) then
            table.insert(failures, string.format("%s: expected %s, got %s", what, vim.inspect(wanted), vim.inspect(actual)))
          end
        end

        local finished = false

        local function finish()
          if finished then
            return
          end
          finished = true

          -- Then: nothing raised an error along the way. This catches a broken
          -- Lua chunk, not a server that fails to start - none is started here.
          local messages = vim.api.nvim_exec2("messages", { output = true }).output
          if messages:match("E%d+:") or messages:lower():match("error") then
            table.insert(failures, "messages: " .. messages)
          end

          if #failures > 0 then
            io.stderr:write("nvim-${name}: " .. #failures .. " assertion(s) failed\n")
            for _, failure in ipairs(failures) do
              io.stderr:write("  - " .. failure .. "\n")
            end
            vim.cmd("cquit 1")
          end

          vim.fn.writefile({ "ok" }, vim.env.NVIM_CHECK_SENTINEL)
          io.stdout:write("nvim-${name}: OK\n")
          vim.cmd("qa!")
        end

        vim.defer_fn(function()
          table.insert(failures, "timed out: the check never called finish()")
          finish()
        end, 30000)

        ${body}
      '';
    in
    pkgs.runCommand "nvim-${name}-check" { nativeBuildInputs = [ nvim ]; } ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME"
      cd "$TMPDIR"

      ${fixtures}

      export NVIM_CHECK_SENTINEL="$TMPDIR/passed"
      nvim --headless -c 'luafile ${script}'

      if [ ! -f "$NVIM_CHECK_SENTINEL" ]; then
        echo "nvim-${name}: the assertions did not run to completion" >&2
        exit 1
      fi

      touch $out
    '';
in
{
  typescript = mkCheck {
    name = "typescript";

    fixtures = ''
      # A classic root: the era probe looks for exactly this file, and nothing
      # reads its contents.
      mkdir -p classic/node_modules/typescript/lib
      touch classic/pnpm-lock.yaml classic/node_modules/typescript/lib/tsserver.js
      touch classic/a.ts

      # A native root: TypeScript 7 ships no tsserver.js, only the tsc binary.
      mkdir -p native/node_modules/.bin
      touch native/pnpm-lock.yaml
      printf '#!/bin/sh\nexit 0\n' > native/node_modules/.bin/tsc
      chmod +x native/node_modules/.bin/tsc
      touch native/a.ts

      # Belongs to no project: no lockfile and no .git above it.
      touch loose.ts
    '';

    body = ''
      local servers = { "vtsls", "tsc" }

      -- What each server claims for one file: its root, or nil where it
      -- declines. Both are asked about the same buffer, which is the situation
      -- being tested.
      --
      -- The buffer is unlisted but NOT scratch: `vim.fs.root` returns nil for
      -- any buffer whose buftype is not "", so a scratch buffer would make every
      -- root_dir fall through to its cwd fallback and the whole check pass
      -- vacuously. Nothing is `:edit`ed and no filetype is set, so no language
      -- server attaches inside the sandbox.
      local function claimedRoots(file)
        local bufnr = vim.api.nvim_create_buf(false, false)
        vim.api.nvim_buf_set_name(bufnr, file)

        local claimed = {}
        for _, server in ipairs(servers) do
          vim.lsp.config[server].root_dir(bufnr, function(dir)
            claimed[server] = dir
          end)
        end

        return claimed
      end

      -- Given: one project root per era, plus a file belonging to neither.
      local cwd = vim.fn.getcwd()
      local classic = cwd .. "/classic"
      local native = cwd .. "/native"

      local routing = {
        { file = classic .. "/a.ts", server = "vtsls", root = classic },
        { file = native .. "/a.ts", server = "tsc", root = native },
        -- No lockfile and no .git anywhere above it: the era probe falls back to
        -- the working directory, finds no classic service, and the native server
        -- takes it. This is the loose-scratch-file path.
        { file = cwd .. "/loose.ts", server = "tsc", root = cwd },
      }

      -- Then: the expected server claims the root and the other one declines.
      -- Asserting both directions is the point - two servers claiming the same
      -- root means duplicate diagnostics and duplicate completions.
      for _, case in ipairs(routing) do
        local claimed = claimedRoots(case.file)
        for _, server in ipairs(servers) do
          local wanted = (server == case.server) and case.root or nil
          expect(string.format("%s claims %s", server, case.file), claimed[server], wanted)
        end
      end

      -- Then: the native server prefers the project's own tsc, and falls back to
      -- the one on PATH for a root that has none.
      local binaries = {
        { root = native, wanted = native .. "/node_modules/.bin/tsc" },
        { root = classic, wanted = "tsc" },
      }

      for _, case in ipairs(binaries) do
        expect("tsc binary for " .. case.root, _G.TsNativeBinary(case.root), case.wanted)
      end

      finish()
    '';
  };

  search = mkCheck {
    name = "search";

    # One file with "target N" on every 10th line, so there are several
    # matches to move between and each one's line number is known.
    fixtures = ''
      for i in $(seq 1 120); do
        if [ $((i % 10)) -eq 0 ]; then echo "target $((i / 10))"; else echo "filler line $i"; fi
      done > fixture.txt
    '';

    body = ''
      local config = require("snacks.picker.config")

      -- Then: telescope still owns `vim.ui.select`, and therefore every
      -- code-action dialog. snacks defaults this to `true`, so a misspelled
      -- key in the freeform `settings.picker` would silently hand it over.
      expect("ui_select", config.get({}).ui_select, false)

      -- Then: live searches stop at 1000 matches rather than upstream's
      -- 10000, which is what made live grep lag. `lines` is not live, so it
      -- is unaffected by construction.
      expect("limit_live", config.get({}).limit_live, 1000)

      -- Then: everything else about the grep sources is stock. These are not
      -- set anywhere; asserting them means a plugin bump that changes one
      -- fails the build instead of silently changing the DX.
      local stock = {
        { source = "grep", key = "live", want = true },
        { source = "git_grep", key = "live", want = true },
        { source = "grep_word", key = "live", want = false },
        { source = "grep", key = "hidden", want = nil },
        { source = "git_grep", key = "untracked", want = false },
      }
      for _, case in ipairs(stock) do
        expect(
          string.format("%s.%s (stock)", case.source, case.key),
          config.get({ source = case.source })[case.key],
          case.want
        )
      end

      -- `lines` keeps its own preview-in-the-editor-window layout.
      local lines_layout = config.get({ source = "lines" }).layout
      expect("lines.layout.preset", lines_layout.preset, "ivy")
      expect("lines.layout.preview", lines_layout.preview, "main")

      -- Then: every content-search key dispatches to its snacks source. The
      -- leader is a space, so that is what the stored lhs looks like.
      local keymaps = {
        { key = " fd", mode = "n", want = "Snacks.picker.grep()" },
        { key = " fg", mode = "n", want = "Snacks.picker.git_grep()" },
        { key = " fw", mode = "n", want = "Snacks.picker.grep_word()" },
        { key = " fw", mode = "x", want = "Snacks.picker.grep_word()" },
        { key = " ss", mode = "n", want = "Snacks.picker.lines()" },
        { key = " fl", mode = "n", want = "Snacks.picker.resume()" },
      }
      for _, case in ipairs(keymaps) do
        local rhs = (vim.fn.maparg(case.key, case.mode, false, true) or {}).rhs or ""
        expect(
          string.format("%q (%s) dispatches to %s", case.key, case.mode, case.want),
          rhs:find(case.want, 1, true) ~= nil,
          true
        )
      end

      -- Then: enabling the picker did not drag the rest of the snacks suite
      -- in with it.
      expect("picker enabled", Snacks.config.picker.enabled, true)
      for _, snack in ipairs({ "dashboard", "explorer", "notifier", "input", "scroll" }) do
        expect(snack .. " enabled", (Snacks.config[snack] or {}).enabled, nil)
      end

      -- The bug this guards: in stock snacks <Tab> is `select_and_next`, so
      -- tabbing down to the 5th match of <leader>ss selected matches 1-4 on
      -- the way and <CR> jumped to the FIRST of them. Driven with real
      -- keystrokes through the real keymap, because a config assertion alone
      -- would not show that the jump lands on the right line.
      --
      -- Each scenario: open fixture.txt at line 1, run its keys with a pause
      -- between each so the picker can open and match, then assert.
      local function feed(keys)
        vim.api.nvim_feedkeys(vim.keycode(keys), "mt", false)
      end

      local function scenario(keys, assert_fn, done)
        vim.cmd("silent! %bwipeout!")
        vim.cmd("edit fixture.txt")
        vim.api.nvim_win_set_cursor(0, { 1, 0 })
        vim.fn.setqflist({})

        local i = 0
        local function step()
          i = i + 1
          if keys[i] then
            feed(keys[i])
            vim.defer_fn(step, 250)
          else
            vim.defer_fn(function()
              assert_fn()
              done()
            end, 400)
          end
        end
        step()
      end

      local function cursor_line()
        return vim.api.nvim_win_get_cursor(0)[1]
      end

      -- Given the matches on lines 10, 20, 30, 40, 50, ...
      local open = { " ss", "target" }
      local scenarios = {
        -- When <Tab> moves four times and <CR> confirms,
        -- Then the cursor is on the 5th match, not the 1st.
        {
          keys = vim.list_extend(vim.deepcopy(open), { "<Tab>", "<Tab>", "<Tab>", "<Tab>", "<CR>" }),
          check = function() expect("<Tab> x4 then <CR> lands on", cursor_line(), 50) end,
        },
        -- When <S-Tab> walks back up from there,
        -- Then it moves too, rather than deselecting.
        {
          keys = vim.list_extend(vim.deepcopy(open), { "<Tab>", "<Tab>", "<Tab>", "<S-Tab>", "<CR>" }),
          check = function() expect("<Tab> x3, <S-Tab>, <CR> lands on", cursor_line(), 30) end,
        },
        -- When <C-Space> selects two matches and <C-q> sends them,
        -- Then selection still works on its new key and reaches the quickfix list.
        {
          keys = vim.list_extend(vim.deepcopy(open), { "<C-Space>", "<C-Space>", "<C-q>" }),
          check = function()
            local lines = vim.tbl_map(function(e) return e.lnum end, vim.fn.getqflist())
            expect("<C-Space> x2 then <C-q> quickfix lines", lines, { 10, 20 })
          end,
        },
      }

      local function run(n)
        local s = scenarios[n]
        if not s then
          return finish()
        end
        scenario(s.keys, s.check, function() run(n + 1) end)
      end

      -- Start once startup has settled, so <leader>ss is mapped and snacks is up.
      vim.defer_fn(function() run(1) end, 200)
    '';
  };
}
