{ pkgs }: pkgs.writeShellScriptBin "hello" (builtins.readFile ./hello.sh)
