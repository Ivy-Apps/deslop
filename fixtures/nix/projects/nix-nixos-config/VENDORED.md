# Vendored fixture: nix-nixos-config

- Source: https://github.com/ILIYANGERMANOV/nixos
- Commit: ad99a66
- Vendored: 2026-10-24
- License: MIT, see `LICENSE` (copied verbatim)

This is a scrubbed snapshot for the Deslop test suite. It preserves the
dependency graph exactly: every `.nix` file at its original path, every path
literal and import in them unchanged, and every non-`.nix` file a `.nix` file
references by path still present at that path.

What was scrubbed:

- Every `.nix` file is verbatim except:
  - email addresses in `hosts/*/configuration.nix` replaced with
    `example.com` addresses;
  - SSH public keys (`hosts/software-factory/configuration.nix` admin keys,
    the GitHub host key in `modules/nixos/server/ssh.nix`) replaced with fake
    `ssh-ed25519 AAAA...fixture` strings.
- Referenced non-`.nix` files are one-line stubs: the shell scripts under
  `programs/software-factory/{ctl,ops}/` and `check.sh`, `AGENTS.md`,
  `instructions.md`, `admin/CLAUDE.md`, `skills/custom/README.md`, and the
  sops-encrypted `secrets/**/*.yaml` (ciphertext and age recipients removed).
- Skill directories read with `builtins.readDir` or walked for `SKILL.md`
  (`programs/agents/skills/custom/*`, `programs/software-factory/admin/skills/*`)
  keep one stub `SKILL.md` each, with only `name` and `description`
  frontmatter.
- `tailscale/policy.hujson` is a stub.
- Everything else was dropped: docs, ADRs, README, CLAUDE.md, CONTEXT.md,
  flake.lock, Justfile and `just/`, `.github/`, `.sops.yaml`, `.typos.toml`,
  `.gitignore`, and the skills' `references/` files.

What was added:

- `deslop/nix.yaml` and `deslop/rules/architecture.yaml` were added by Deslop
  for its tests and are not from the source repository.
