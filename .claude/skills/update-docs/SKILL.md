---
name: update-docs
description: Document the Rules DSL changes that landed between two Deslop releases. Reads the merged PRs in a tag range, works out what changed in the DSL as a language, updates llms.txt, README.md and examples/rules, verifies every new claim by probing the real CLI, and opens a draft PR in each repo it touched. Use for "/update-docs", "update the docs for the release", "document what changed in the DSL since <tag>".
---

# Update Deslop docs

You are documenting a **language**. Deslop's Rules DSL is the product; the docs
teach a reader to write a correct rulebook. Everything else in the repo, however
much it changed, is out of scope.

Two invariants hold for the whole run:

- **A claim you have not observed is not a claim you may write.** The CLI is
  available locally; run it. "Verified behaviour" in these docs is literal.
- **You may not touch a file before the user has approved the plan.** Phases 1
  and 2 are read-only apart from throwaway probe projects in the scratchpad.

Ask the user questions **one at a time** through `AskUserQuestion`, with your
recommended answer first and labelled `(Recommended)`.

---

## 0. Arguments

`/update-docs [tips] [tags range]`

Parse positionally: if the **last** whitespace-separated token parses as
`<ref>..<ref>`, it is the range. Everything before it is `tips`. Either may be
absent.

**Default range** is `<immediately preceding tag>..<latest tag>`:

```bash
git fetch --tags --quiet
LATEST=$(git tag --sort=-creatordate | head -1)
PREV=$(git tag --sort=-creatordate | sed -n 2p)
```

No walking back to find changes, no minor-version cleverness. The user runs this
when there is something to document, and passes an explicit range when the
default is not what they want. Print the range you resolved before doing
anything else.

**Tips are high-priority steering hints.** They may focus the run on one
feature, declare a doc stale beyond the range, explain what a feature does (which
pre-answers a Phase 2 question), or veto a file. They outrank your own inference.
They never suppress the commit analysis: you still read the whole range and still
report anything the tips did not mention.

---

## 1. Gather

Read, in this order. Every one of these is a human being stating what the
behaviour is, which is worth more than inferring it from a diff.

```bash
git log --oneline "$PREV..$LATEST"                 # PR numbers are in the subjects
gh pr view <n> --json title,body,files             # for each merged PR in range
git diff "$PREV..$LATEST" -- docs/adr              # new ADRs are the design record
git diff "$PREV..$LATEST" -- docs/GLOB+.md         # the pattern semantics, already written up
git diff "$PREV..$LATEST" -- .golden               # real CLI output, before and after
git diff "$PREV..$LATEST" -- fixtures              # real rulebook YAML the tests run
```

Drop into `src/` only to settle a specific question the above left open. A range
is routinely 40 files and 3000 lines of Haskell, nearly all of it internal.

### What is in scope

Only **Rules DSL language changes**: a new keyword, a new pattern token, a new
clause, a change in what an existing construct *means* or *matches*, a change in
what the CLI requires the user to configure, a change in what a violation prints.

Out of scope: internal refactors, performance work, CI, bugfixes that restore
documented behaviour. A bugfix that changed what a rulebook matches is **in**
scope, because the language now means something different; say so as semantics,
not as a fix.

The test to apply per PR: *would a rulebook author write something different
because of this?* If no, drop it and say why in your report.

### Probe to learn

Where the PRs and ADRs leave the semantics ambiguous, do not infer and do not
guess: build a probe and find out. See the probe recipe below. Writing from
observation is what makes Phase 2 short.

---

## 2. Clarify

Collect every remaining unknown, then ask them all before writing a plan. Never
start an edit while something is unknown, and never scatter these questions
through the run.

Two kinds:

- **Semantics.** What is this feature for, what problem did it solve, what should
  a reader do with it. The code says what it does, not why it exists.
- **Structure**, whenever `llms.txt` needs reorganising rather than extending.
  Restructuring is allowed, but you must put the structural choice to the user
  before the final plan, not present it as settled.

---

## 3. Plan, and wait

Present a plan and **stop**. It states:

- Each DSL change found, and the PR it came from.
- Each PR in range you judged out of scope, one line each, with the reason.
- Per file: what the edit will say, in enough detail that approving it is a real
  decision rather than a formality.
- If `llms.txt` is being restructured: which sections move, why the current
  structure fails the new feature, and the resulting outline.

Do not touch a file until the user approves.

---

## 4. Write

### The doc surfaces

| File | Repo | Audience |
|---|---|---|
| `../deslop-web/public/llms.txt` | `deslop-web` | Coding agents |
| `README.md` | `deslop` | Human developers |
| `examples/rules/*.yaml` | `deslop` | Anyone copying a rulebook |
| `examples/rules/README.md` | `deslop` | The examples index |

`docs/GLOB+.md`, `docs/adr/` and `CONTEXT.md` are **inputs, never outputs**.
They are technical docs, maintained by the PR that changes the behaviour. If you
find one of them stale, say so in your summary and leave it alone.

### llms.txt

The file an agent reads to learn the DSL with no other page to fetch. It is
exhaustive on purpose: edge cases, limitations, the failure modes, and evidence.

- Prefer including everything an agent needs over keeping it short.
- Match the existing voice: declarative, evidence-backed, no hedging. It states
  what happens and, where it matters, that this was verified.
- Update the tables, not only the prose. A new clause or pattern token appears in
  the rule-fields table, the "where each pattern type is legal" table and the
  self-check list, or an agent reading only the tables will miss it.
- Bump every version stamp to the range's end tag. There are several, including
  claims phrased as "0.10.x does not print it"; `grep -n '0\.1[0-9]'` finds them.
- You may restructure, having agreed the structure in Phase 2.

### README.md

For humans. Friendlier and shorter than `llms.txt`, and it delegates depth to
`docs/GLOB+.md`. Do not mirror `llms.txt` section for section.

The two must never contradict each other. After editing both, re-read the pair
on the changed feature and confirm they agree.

### examples/rules

These are teaching material that people copy, not a feature showcase.

Adopt new syntax **only where it genuinely reads better** for that rule, for
example a longhand `forbids: "**"` plus `allows:` that `allows-only:` says more
clearly. Leave the rest alone. If a change adds or removes a rulebook file,
update the table in `examples/rules/README.md` and the one in `README.md`.

Every rulebook you touch gets probed in Phase 5: it must load, and it must still
fire on the violation it was written to catch.

---

## 5. Verify

Spawn **one** background agent (`Agent`, `subagent_type: "general-purpose"`) and
brief it **blind**.

It receives a numbered list of testable claims and the repo path. It does **not**
receive your prose, your diff, or your reasoning. An agent that reads the
sentence probes the case the sentence describes; an agent handed only the claim
has to design the probe itself, which is what catches a claim that is true as
written but misleading.

Extract claims as standalone yes/no propositions:

```
1. `allows-only:` forbids unresolved npm package imports such as `react`.
2. `..*` in a clause pattern resolves once per ancestor directory and matches if any resolution matches.
3. `..*` is rejected in an `exists:` clause.
```

Ask it to return, per claim: **confirmed**, **contradicted** or
**could-not-probe**, each with the probe files and the verbatim CLI output.
Tell it to report only and change nothing.

Its scope is every claim written or edited in this run, plus a load-and-fire
probe for each `examples/rules/*.yaml` the run touched.

While it runs, do useful work: re-read the full diff, check internal links and
anchors still resolve, and confirm the README and llms.txt agree.

**You are not done until the report lands and you have acted on it.** Fix every
contradicted claim yourself. A `could-not-probe` verdict is not a pass: either
probe it another way or drop the claim.

### Probe recipe

A probe is a throwaway TypeScript project in the scratchpad. Never in
`fixtures/`, which belongs to the test suite.

```bash
PROBE="$SCRATCHPAD/probe"
mkdir -p "$PROBE/src" "$PROBE/deslop/rules"
echo '{ "compilerOptions": { "baseUrl": ".", "paths": { "@/*": ["./src/*"] } } }' > "$PROBE/tsconfig.json"
# write the .ts files and deslop/rules/probe.yaml, then, from the repo root:
nix develop -c cabal run deslop -- check "$PROBE"
```

Build from the checkout, never `npx`: the published package lags the code the
range documents.

**A rule that matches nothing passes.** A green run proves nothing on its own, so
every probe needs both a file that must violate and a file that must not, and
both verdicts have to be confirmed. Check the `Checked N modules enforcing M
rules` line: if `N` is 0, the alias is wrong and nothing below it means anything.

---

## 6. Ship

If any `examples/rules/*.yaml` changed, run the full suite and require green:

```bash
nix run .#test
```

Not a targeted spec. `LoaderSpec` is what asserts the shipped examples load
today, but that is a detail of the current test layout, and a refactor should not
be able to quietly remove your gate.

Then, in **each repo that has changes**, branch, commit, push and open a **draft**
PR:

| | `deslop` | `deslop-web` |
|---|---|---|
| Branch | `docs/v<end-tag>` | `docs/v<end-tag>` |
| Title | `[Docs] Document the v<end-tag> Rules DSL` | `llms.txt: v<end-tag> Rules DSL` |

The `[Scope]` prefix is this repo's commit convention; `deslop-web` uses plain
titles. For a range spanning several tags, name the branch `docs/v<start>-v<end>`.

The `deslop` PR body follows `.github/pull_request_template.md`. `CONTRIBUTING.md`
says a docs-only PR deletes the QA & Correctness Proof section, but this run has
something better than "docs only": fill that section with the verification
agent's evidence, one entry per claim with its rulebook and CLI output. List
which DSL changes are documented and which PRs they came from.

Finish by telling the user both PR URLs, what you documented, what you dismissed
as out of scope, and anything the verifier could not settle.
