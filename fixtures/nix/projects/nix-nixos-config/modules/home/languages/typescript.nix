{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nodejs_24
    pnpm
    # TypeScript <= 6, where the language service is still lib/tsserver.js.
    # Points at the project's own copy of it, never this one.
    vtsls
    # TypeScript >= 7, where the language service is `tsc --lsp`. Only the
    # fallback: an installed project's own tsc is preferred over it.
    typescript
    vscode-langservers-extracted
    biome
  ];
}
