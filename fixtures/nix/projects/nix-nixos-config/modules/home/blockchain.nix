{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # forge, cast, anvil and chisel - build/test, chain RPC, local devnet, REPL.
    foundry
    # Standalone solc. Foundry resolves its own compiler through svm, which
    # downloads the version a pragma asks for into
    # ~/Library/Application Support/svm and therefore needs network; this one is
    # the offline fallback (`forge build --use "$(command -v solc)"`) and what
    # non-foundry tooling (slither, editors, scripts) compiles with.
    solc
    # Static analysis for Solidity. Brings solc-select along for pinning the
    # compiler version a contract expects.
    slither-analyzer
  ];
}
