# npm packages ship prebuilt Linux binaries - Next.js' SWC, esbuild,
# Playwright's Chromium - that expect a standard dynamic loader and /usr/lib.
# nix-ld provides both, so agents can `npm install` and
# `npx playwright install` in any web project without per-project Nix work.
{ pkgs, ... }:
{
  programs.nix-ld = {
    enable = true;
    # The module's defaults cover most native Node addons. These are what a
    # headless Chromium needs on top of them.
    libraries = with pkgs; [
      alsa-lib
      at-spi2-atk
      at-spi2-core
      cairo
      cups
      dbus
      expat
      glib
      gtk3
      libdrm
      libgbm
      libx11
      libxcb
      libxcomposite
      libxdamage
      libxext
      libxfixes
      libxkbcommon
      libxrandr
      nspr
      nss
      pango
    ];
  };
}
