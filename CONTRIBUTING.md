# Contributing to Deslop

Deslop is a small project with a high bar. The rules below exist so that review
time goes to work that can merge, and so that you know before you start whether
your work will be reviewed at all.

The short version:

- **Docs, examples and the landing page:** just open the PR. Genuinely wanted.
- **Code:** get the issue green-lit first, then build it.
- **Every PR:** proves that it works, and its author can defend every line.

## Development setup

Deslop is Haskell built with [Nix](https://nixos.org/), and nothing builds
outside the dev shell, so set that up before anything else:

**→ [docs/DEVELOPMENT.md](docs/DEVELOPMENT.md)** - setup, commands, project
layout, coding conventions.

## Easiest places to help

- **Docs** - the README, `docs/`, or anything that confused you when you first
  used Deslop. If it confused you, it confuses everyone.
- **[`examples/rules/`](examples/rules/)** - a rulebook for a stack we don't
  cover yet.
- **[deslop.dev](https://deslop.dev)** - the landing page lives in
  [Ivy-Apps/deslop-web](https://github.com/Ivy-Apps/deslop-web). Copy, design,
  examples, accessibility: all fair game.
- **Bug reports** with a minimal reproducer. A rulebook plus the smallest
  TypeScript project that shows the wrong output is worth more than a long
  description.

These need no issue and no ceremony. Open the PR. They are genuinely wanted, not
a consolation prize.

## Before you open a PR

| Tier | What | What you do |
|---|---|---|
| **Free** | Docs, `examples/rules/`, typos, deslop-web | Open the PR. |
| **Issue-first** | Bug fixes, new rules, CLI or output changes, refactors | Open an issue (or find one), say what you plan to do, wait to be assigned. |
| **Ask first** | New language frontends, architecture changes, new dependencies | Open an issue and discuss. Assume the answer is no unless a maintainer says otherwise. |

**A maintainer assigning the issue to you is the green light.** It is also how
we avoid two people building the same thing.

```
You:  "I'd like to take this. Plan: <two or three sentences>."
Us:   assigns it to you   -> build it
      or says no          -> nothing lost, no PR wasted
```

Assigned to someone else means it is taken; ask before duplicating. Assigned to
you but silent for a long time means we may unassign it.

A PR in the issue-first or ask-first tiers with no green-lit issue may be closed
without review. Not because the work is bad, but because reviewing code nobody
asked for is the single easiest way to burn a small project's maintainers.

## AI and ownership

Use whatever tools you like. The bar does not move, and we are not going to ask
what wrote your code.

**You are solely responsible for your contribution, including the parts an agent
wrote.** In review you will be asked why a line exists, why this design and not
another, what happens on the unhappy path. "The AI did it" is not an answer. A
PR you cannot defend gets closed.

The same applies to everything you write around the code. Your PR description
and your review replies must be written and understood by you. Pasting a wall of
agent prose into a review thread moves your work onto the reviewer, which is the
one thing this whole document exists to prevent. Short and specific beats long
and generated, every time.

If you are working with an agent, point it at [`CLAUDE.md`](CLAUDE.md) before it
writes anything. It is the fastest description of how this codebase is built.
Then review its output yourself, properly, before you open the PR.

## The bar

No tech debt. No shortcuts. No "I'll clean it up in a follow-up". This project
is meant to stay fun to work on, not turn into a pile of legacy code nobody
wants to touch.

Concretely, a change merges when:

- It fits the layering and module conventions in [`CLAUDE.md`](CLAUDE.md) and
  the decisions in [`docs/adr/`](docs/adr/).
- It reads like the code around it.
- Anything architectural comes with an ADR in the same PR.
- `nix run .#build`, `nix run .#test` and `nix run .#lint` all pass. The build
  is `-Wall -Werror`; formatting is fourmolu.
- It proves it works. See [Correctness proof](#correctness-proof).

Whether a PR is high enough quality and effort to merge is a subjective decision
by the Deslop team. There is no appeal process, and there is no obligation to
review or merge anything.

## Review process

### Rounds

1. **You** open the PR: CI green, template filled in, proof included.
2. **Round 1** is a human high-level pass on scope, design and whether the proof
   is real, and an in-depth AI review, delivered together. A PR can be closed on
   the human pass alone, before the AI review ever runs. Wrong-by-design does
   not earn a line-by-line review.
3. **You** address every comment, then re-request review.
4. Repeat until there is nothing left to say, then a maintainer merges.

Multiple rounds are normal. A second round is not a sign that something went
wrong.

### Comments

Every comment ends one of two ways.

**Fixed** - reply with a link to the commit that fixes it, then resolve the
thread yourself. A maintainer re-opens it if the fix misses.

```
Fixed in https://github.com/Ivy-Apps/deslop/pull/123/commits/<sha>
```

**Pushed back** - reply with the reason and **leave the thread open**. Only a
maintainer can accept a push-back. Disagreeing is fine and sometimes you will be
right; silently ignoring is not.

A reply of "done", "fixed" or "good catch, updated" with no commit link does not
count. Anything unaddressed means the round failed and the PR is not ready.

### Re-requesting review

When every comment is handled, press GitHub's **Re-request review** button. That
is the only signal that counts. A comment saying "ready" notifies nobody, and a
PR that never re-requests review never gets looked at again.

Do not re-request with comments still outstanding. That is the fastest way to
burn your next round.

### History and conflicts

**During a review round, push fix commits on top.** Do not amend, squash or
otherwise rewrite history mid-round: it orphans the review threads and breaks
the commit links you just posted. History gets tidy at the end, because
maintainers squash-merge.

**Rebase on `origin/main` to resolve conflicts,** never merge `main` in:

```bash
git fetch origin
git rebase origin/main
git push --force-with-lease
```

A PR with merge conflicts is not re-reviewed until they are resolved, and the
staleness clock keeps running while it sits there.

### Stale PRs

A PR with no activity for 4 days is labelled stale, and closed 3 days after
that. Any push or comment clears the label.

Closing is not a rejection. Push your fixes and reopen it.

## Correctness proof

Every PR that changes behaviour has to show that it works. Reviewers should not
have to take your word for it, and "it compiles" is not evidence.

The PR template ends with a **QA & Correctness Proof** section:

- **Reproducible proof** - unit tests covering happy, unhappy and edge paths,
  property-based tests, and E2E tests. Check the box and point at the test, or
  leave it unchecked with a one-line reason on the same line. An unchecked box
  with no reason means the PR is not ready and will not be reviewed.
- **Manual proof** - what you actually did to convince yourself: a demo, a
  video, terminal output, a link to a green CI run.

Docs-only PRs delete the whole section and say "docs only".

Tests mirror `src/`, and E2E golden tests live in `test/E2E/` with snapshots in
`.golden/`. See [docs/DEVELOPMENT.md](docs/DEVELOPMENT.md#testing).

## Legal

By opening a PR you confirm that you have the right to contribute the code: that
you wrote it or otherwise hold the rights to it, that it does not violate anyone
else's copyright, licence or other rights, and that you are legally responsible
for what you submit. This holds regardless of what tools you used to produce it.

Contributions are licensed under the [MIT licence](LICENSE), like the rest of
the project.

---

Be civil, assume good faith, and see [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).
