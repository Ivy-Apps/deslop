_:

{
  keymaps = [
    {
      mode = "n";
      key = "<leader>fy"; # "Find Yank"
      action = "<cmd>Telescope neoclip<CR>";
      options.desc = "Clipboard History (Telescope)";
    }

    # --- Non-destructive editing ---
    # We keep `clipboard=unnamedplus` so plain `y`/`p` still use the system
    # clipboard (essential for copying out to the browser/Slack). The mappings
    # below stop *destructive* edits from silently overwriting that clipboard.
    # Visual-mode maps use "x" (visual only, not select mode) so typing over a
    # snippet placeholder in select mode still inserts the literal character.

    # 1. Paste over a visual selection without swapping the register.
    # Deletes the selection to the black hole first, so the pasted text (e.g. a
    # secret from Bitwarden) stays in the clipboard and can be pasted again.
    {
      mode = "x";
      key = "p";
      action = ''"_dP'';
      options.desc = "Paste over selection (keep clipboard)";
    }

    # 2. Change to end of line discards the replaced text.
    {
      mode = [
        "n"
        "x"
      ];
      key = "C";
      action = ''"_C'';
      options.desc = "Change to EOL (discard old text)";
    }

    # 3. All change operators (c, cc, ciw, ...) discard the replaced text.
    {
      mode = [
        "n"
        "x"
      ];
      key = "c";
      action = ''"_c'';
      options.desc = "Change (discard old text)";
    }
    {
      mode = [
        "n"
        "x"
      ];
      key = "s";
      action = ''"_s'';
      options.desc = "Substitute (discard old text)";
    }

    # 4. Normal-mode single-char delete discards, so tapping `x` on a stray char
    # never clobbers the clipboard (note: breaks the `xp` transpose idiom).
    # Visual mode is intentionally left vanilla: `Vx` is a deliberate *cut* and
    # should land the selection in the clipboard so it can be moved/pasted.
    {
      mode = "n";
      key = "x";
      action = ''"_x'';
      options.desc = "Delete char (discard)";
    }
  ];

  clipboard.register = "unnamedplus";

  plugins = {
    neoclip = {
      enable = true;
      settings = {
        history = 100;
      };
    };
  };
}
