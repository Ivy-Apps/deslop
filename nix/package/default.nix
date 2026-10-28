# The released Deslop binary, as the GitHub Release ships it. It is not built
# from source: the release workflow already produces static binaries, and a
# source build would make every user compile the whole Haskell closure. See
# docs/adr/0025-nix-installs-the-released-binary.md.
#
# The version and one download URL and hash per system live in release.json,
# which .github/workflows/nix-bump.yml rewrites after each release.
{ lib, stdenvNoCC, fetchurl }:
let
  release = lib.importJSON ./release.json;
  inherit (stdenvNoCC.hostPlatform) system;
  platform = release.platforms.${system}
    or (throw "deslop: no release binary for ${system}");
in
stdenvNoCC.mkDerivation {
  pname = "deslop";
  inherit (release) version;

  src = fetchurl {
    inherit (platform) url hash;
  };

  # The tarball holds the bare `deslop` binary, with no directory around it.
  sourceRoot = ".";
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 deslop $out/bin/deslop
    runHook postInstall
  '';

  meta = {
    description = "Static import-graph analyzer: architecture rules in YAML, checked on every run";
    homepage = "https://deslop.dev";
    license = lib.licenses.mit;
    mainProgram = "deslop";
    platforms = builtins.attrNames release.platforms;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
