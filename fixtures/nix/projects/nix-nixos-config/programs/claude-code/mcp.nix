# Pure MCP catalog logic: declaring servers, resolving them against the secrets
# a host provides, and picking one flavor's subset. No derivations and no
# wrappers, which is what lets programs/claude-code/check.nix exercise all of it
# against a synthetic catalog and a synthetic `secrets`.
#
# `secrets` is the name -> decrypted-file path attrset for the secrets THIS host
# provides. Paths only, never values.
{
  lib,
  secrets ? { },
}:

let

  # A catalog entry is one of two transports, tagged by `type`. The tag is the
  # same field Claude Code reads from ~/.claude.json, so writing an entry out is
  # a projection rather than a translation.

  # Constructs a local server, which Claude Code spawns as a subprocess.
  #
  # `env` maps an environment variable to the NAME of the secret that fills it,
  # never to a path and never to a value: which hosts provide that secret is
  # decided by modules/secrets.nix, and this layer only ever sees the
  # name-to-path attrset it was handed, so programs/ stays host-agnostic.
  #
  # A server needing no secrets declares no `env`. That is an empty attrset
  # rather than a special case: every rule below reads as "for all the secrets
  # this server names", which holds vacuously for none.
  mkStdioServer =
    {
      command,
      args,
      env ? { },
    }:
    {
      type = "stdio";
      inherit command args env;
    };

  # Constructs a remote server, which Claude Code connects to over HTTP.
  #
  # Remote servers authenticate out-of-band, so there is no secret to inject and
  # nothing here to gate on. Claude Code runs the OAuth flow on first use
  # (`/mcp`) and stores the grant in the macOS Keychain - on Linux, in
  # ~/.claude/.credentials.json - under a key derived from the server's name,
  # type, url and headers. None of it lives in ~/.claude.json, which is why the
  # wrapper replacing .mcpServers wholesale on every launch cannot lose it.
  # Renaming a catalog entry or changing its url DOES orphan the grant, costing
  # one re-login.
  #
  # `env = { }` is not a placeholder: it is what makes availability hold
  # vacuously, so a remote server is available on every host and none of the
  # rules below need a case for it.
  #
  # There is no `headers` argument because nothing needs one yet. Claude Code
  # expands ${VAR} references in `headers` exactly as it does in `env`, so a
  # remote server that ever wants a static bearer token is a small addition
  # here, not a redesign.
  mkHttpServer =
    { url }:
    {
      type = "http";
      inherit url;
      env = { };
    };

  # The servers this host can actually run, with every secret name resolved to
  # its decrypted-file path.
  #
  # A server whose secret the host does not declare is dropped here, so a flavor
  # listing it gets nothing at all rather than a server fed a placeholder token.
  # Availability is all-or-nothing: a server naming two secrets needs both.
  availableServers = catalog: lib.mapAttrs (_: resolve) (lib.filterAttrs (_: isAvailable) catalog);

  isAvailable = server: lib.all (name: secrets ? ${name}) (lib.attrValues server.env);

  resolve = server: server // { env = lib.mapAttrs (_: name: secrets.${name}) server.env; };

  # The subset of `available` that one flavor asked for.
  #
  # Requests are validated against every DEFINED name and then read from the
  # AVAILABLE ones, so a typo aborts evaluation the way an unknown skill name
  # does, while a server this host has no secret for stays silent - which is the
  # point of dropping it.
  selectServers =
    {
      flavor,
      known,
      available,
      requested,
    }:
    let
      unknown = lib.subtractLists known requested;
    in
    if unknown != [ ] then
      throw ''
        claude flavor "${flavor}": unknown MCP server(s): ${lib.concatStringsSep ", " unknown}.
        Defined in mcpCatalog: ${lib.concatStringsSep ", " known}.
      ''
    else
      lib.filterAttrs (name: _: builtins.elem name requested) available;

  # How each transport is written into ~/.claude.json.
  #
  # Dispatch is an attrset keyed by the tag rather than an if/else chain, so a
  # transport nothing here handles aborts evaluation instead of falling through
  # to a wrong shape.
  #
  # A stdio server's env values are ${VAR} REFERENCES - the literal string
  # "${FIGMA_API_KEY}" - which Claude Code's expandVars resolves from the process
  # environment at startup. Neither the secret values nor the paths they are read
  # from touch ~/.claude.json. "$" + "{" + envVar + "}" produces that literal in
  # Nix without triggering Nix's own string interpolation syntax.
  #
  # A remote server carries no env at all: its `env = { }` exists for the
  # availability rules, and emitting it here would be noise in a file that only
  # needs the type and the url.
  toWire = {
    stdio = server: {
      inherit (server) type command args;
      env = lib.mapAttrs (envVar: _: "$" + "{" + envVar + "}") server.env;
    };

    http = server: { inherit (server) type url; };
  };

  # The mcpServers structure for ~/.claude.json.
  mkMcpStructure = servers: lib.mapAttrs (_: server: toWire.${server.type} server) servers;
in
{
  inherit
    mkStdioServer
    mkHttpServer
    availableServers
    selectServers
    mkMcpStructure
    ;
}
