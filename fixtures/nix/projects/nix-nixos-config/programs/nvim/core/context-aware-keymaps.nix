_:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>tt";
      action = "<cmd>lua _G.SmartRun('test')<CR>";
      options.desc = "Run Tests (context-aware)";
    }
    {
      mode = "n";
      key = "<leader>oi";
      action = "<cmd>lua _G.SmartRun('organize-imports')<CR>";
      options.desc = "Organize Imports (context-aware)";
    }
  ];

  extraConfigLuaPre = ''
    -- Registry of context-aware runners.
    -- Each entry: { detect = fn(cwd) -> bool, run = fn(action) }
    _G.ContextRunners = {}

    _G.RegisterContextRunner = function(runner)
      table.insert(_G.ContextRunners, runner)
    end

    -- Iterates registered runners in order, calls the first that detects
    -- the current project context. Language modules register via
    -- RegisterContextRunner at startup.
    _G.SmartRun = function(action)
      local cwd = vim.fn.getcwd()
      for _, runner in ipairs(_G.ContextRunners) do
        if runner.detect(cwd) then
          runner.run(action)
          return
        end
      end
      vim.notify("No runner for action '" .. action .. "' in: " .. cwd, vim.log.levels.WARN)
    end
  '';
}
