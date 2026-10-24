# Build-time tests for the MCP catalog logic, wired into `nix flake check` (and
# therefore `just check` in CI) as `checks.claude-code`.
#
# mcp.nix is pure - no derivations, no wrappers, no hosts - so every case here
# runs against a synthetic catalog and a synthetic `secrets` attrset rather than
# against whatever the machine running CI happens to provide.
{
  pkgs,
  lib ? pkgs.lib,
}:

let
  inherit (lib.generators) toPretty;

  # Given: a host that declares two of the three secrets this catalog names.
  secrets = {
    figma-token = "/run/secrets/figma-token";
    shared-token = "/run/secrets/shared-token";
  };

  mcp = import ./mcp.nix { inherit lib secrets; };

  catalog = {
    figma = mcp.mkStdioServer {
      command = "npx";
      args = [ "figma-developer-mcp" ];
      env.FIGMA_API_KEY = "figma-token";
    };

    secretless = mcp.mkStdioServer {
      command = "uvx";
      args = [ "mcp-server-time" ];
    };

    twoSecrets = mcp.mkStdioServer {
      command = "node";
      args = [ "two-secret-server" ];
      env = {
        A_TOKEN = "shared-token";
        B_TOKEN = "undeclared-token";
      };
    };

    remote = mcp.mkHttpServer { url = "https://mcp.example.com/mcp"; };
  };

  known = lib.attrNames catalog;
  available = mcp.availableServers catalog;

  # When: a flavor asks for a set of servers by name.
  select =
    requested:
    mcp.selectServers {
      flavor = "test-flavor";
      inherit known available requested;
    };

  # `throw` is lazy, so the value has to be forced inside tryEval to be caught.
  evalFails = value: !(builtins.tryEval (builtins.deepSeq value value)).success;

  cases = [
    {
      name = "a server is available only when every secret it names is declared";
      actual = lib.attrNames available;
      expected = [
        "figma"
        "remote"
        "secretless"
      ];
    }
    {
      name = "an available server has its secret names resolved to paths";
      actual = available.figma.env;
      expected = {
        FIGMA_API_KEY = "/run/secrets/figma-token";
      };
    }
    {
      name = "a server naming no secrets is available with an empty env";
      actual = available.secretless.env;
      expected = { };
    }
    {
      name = "a flavor asking for nothing gets no servers";
      actual = select [ ];
      expected = { };
    }
    {
      name = "a flavor gets exactly the available servers it asked for";
      actual = lib.attrNames (select [
        "figma"
        "secretless"
      ]);
      expected = [
        "figma"
        "secretless"
      ];
    }
    {
      name = "a flavor asking for a known but unavailable server gets nothing";
      actual = select [ "twoSecrets" ];
      expected = { };
    }
    {
      name = "a flavor asking for a server outside the catalog aborts evaluation";
      actual = evalFails (select [ "figmaa" ]);
      expected = true;
    }
    {
      name = "a server with a secret is written as an env var reference, never a path";
      actual = mcp.mkMcpStructure (select [ "figma" ]);
      expected = {
        figma = {
          type = "stdio";
          command = "npx";
          args = [ "figma-developer-mcp" ];
          env.FIGMA_API_KEY = "\${FIGMA_API_KEY}";
        };
      };
    }
    {
      name = "a server with no secrets is written with an empty env";
      actual = mcp.mkMcpStructure (select [ "secretless" ]);
      expected = {
        secretless = {
          type = "stdio";
          command = "uvx";
          args = [ "mcp-server-time" ];
          env = { };
        };
      };
    }
    {
      # A remote server authenticates out-of-band, so there is nothing for a
      # host to declare and nothing to gate on. `secrets` above deliberately
      # holds no key this entry could match.
      name = "a remote server is available on a host that declares none of its secrets";
      actual = available ? remote;
      expected = true;
    }
    {
      name = "a remote server is written as a url, with no env and no command";
      actual = mcp.mkMcpStructure (select [ "remote" ]);
      expected = {
        remote = {
          type = "http";
          url = "https://mcp.example.com/mcp";
        };
      };
    }
    {
      name = "a flavor mixing transports gets both";
      actual = lib.attrNames (select [
        "figma"
        "remote"
      ]);
      expected = [
        "figma"
        "remote"
      ];
    }
    {
      name = "an unavailable local server is still dropped when a remote one is requested alongside";
      actual = lib.attrNames (select [
        "remote"
        "twoSecrets"
      ]);
      expected = [ "remote" ];
    }
  ];

  # Then: every case is reported, so one failure does not hide the rest.
  report =
    {
      name,
      actual,
      expected,
    }:
    if actual == expected then
      ''printf '  ✓ %s\n' ${lib.escapeShellArg name}''
    else
      ''
        printf '  ✗ %s\n' ${lib.escapeShellArg name} >&2
        printf '      expected: %s\n' ${lib.escapeShellArg (toPretty { } expected)} >&2
        printf '      actual:   %s\n' ${lib.escapeShellArg (toPretty { } actual)} >&2
        status=1
      '';
in
pkgs.runCommand "claude-code-check" { } ''
  status=0

  ${lib.concatMapStringsSep "\n" report cases}

  if [ "$status" -ne 0 ]; then
    echo "claude-code: MCP catalog logic is wrong" >&2
    exit 1
  fi

  touch "$out"
''
