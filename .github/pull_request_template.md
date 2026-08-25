<!--
Delete any section that does not apply. The Author Checklist and the
QA & Correctness Proof are the two that stay.

New here? Read CONTRIBUTING.md first - it covers what gets reviewed,
how comments are resolved, and what "proven to work" means.
-->

## What & Why

<!-- What changes, and why it is worth doing. Two or three sentences. -->

Closes #

## How

<!--
Design decisions, trade-offs, anything a reviewer cannot see in the diff.
Delete this section if the diff speaks for itself.
-->

## Author Checklist

- [ ] The issue is green-lit (assigned to me), or this is a free-tier change (docs, examples, typo)
- [ ] Rebased on the latest `origin/main`, no conflicts
- [ ] I understand every line of this diff, can defend it in review, and take full responsibility for it
- [ ] The CI is passing (green).

# QA & Correctness Proof

<!--
Docs-only PR? Delete everything below and write "docs only".

Every box is either checked with a pointer to the test, or unchecked with a
one-line reason on the same line. An unchecked box with no reason means the
PR is not ready and will not be reviewed.
-->

## Reproducible Proof

- [ ] Unit tests for happy, unhappy and edge paths <!-- e.g. test/Deslop/GlobPlusSpec.hs -->
- [ ] Property-based tests <!-- e.g. test/Deslop/GlobPlusPropSpec.hs -->
- [ ] E2E tests <!-- e.g. test/E2E/ + .golden/ -->

## Manual Proof

<!--
What you actually did to convince yourself this works and is correct:
manual QA, a demo video, terminal output, a link to a green CI run.
"It compiles" is not proof.
-->
