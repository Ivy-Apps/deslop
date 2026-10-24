_: {
  nixpkgs.config.allowUnfree = true;

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    auto-optimise-store = true;

    # Isolate builds from the host: no access to your files or the network.
    # "relaxed" keeps normal builds sandboxed, but permits derivations that
    # explicitly opt out (__noChroot / darwin sandbox profile) — e.g. dotnet-sdk,
    # pulled in transitively by pre-commit, which needs network to fetch NuGet.
    sandbox = "relaxed";

    # Block Import From Derivation: evaluating a flake can no longer trigger
    # arbitrary builds/code execution before you approve a build (supply-chain).
    allow-import-from-derivation = false;
    extra-substituters = [ "https://cache.numtide.com" ];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };
}
