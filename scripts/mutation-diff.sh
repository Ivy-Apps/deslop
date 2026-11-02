#!/usr/bin/env bash
# Mutation-tests only what the working tree changes against the merge base with
# <base>, and prints the mutants a reader acts on: survivors, uncovered and
# timed out, with file, line, operator, original and replacement.
#
# Usage: scripts/mutation-diff.sh <base> <check>    (behind `just mutation-diff`)
#
# stdout is the markdown report, and its last line is the path of
# mutation-diff.json, with mutation-diff.md beside it. Every tool log goes to
# run.log in the same directory. Advisory: exits 0 whatever survives, and
# non-zero only when the numbers would be false: a test failing at baseline, a
# runner that died, or a failed control.
#
# Needs nix, git and jq.
set -euo pipefail

BASE="${1:?usage: $0 <base> <check>}"
CHECK="${2:?usage: $0 <base> <check>}"
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"
OUT="$ROOT/dist-newstyle/mutation-diff"
LOG="$OUT/run.log"

fail() {
  {
    echo "mutation-diff: $1"
    echo "mutation-diff: the last lines of $LOG:"
    tail -n 40 "$LOG"
  } >&2
  exit 1
}

# Deleted lines and files outside src/ select no mutants: the tool mutates the
# library alone, and selects by added lines.
not_assessed() {
  local merge_base=$1 others deleted sentence=""
  others=$(git diff --name-only "$merge_base" -- . ':(exclude)src' | wc -l)
  deleted=$(git diff --numstat "$merge_base" -- src | awk '{ d += $2 } END { print d + 0 }')
  if [ "$others" -gt 0 ]; then
    sentence="$others changed files outside src/, tests included, select no mutants."
  fi
  if [ "$deleted" -gt 0 ]; then
    sentence="${sentence:+$sentence }$deleted deleted src/ lines select no mutants."
  fi
  echo "$sentence"
}

rm -rf "$OUT"
mkdir -p "$OUT"
merge_base=$(git merge-base "$BASE" HEAD)

nix build -L ".#checks.x86_64-linux.$CHECK.baseline" --accept-flake-config --no-link >>"$LOG" 2>&1 \
  || fail "the baseline failed: a test fails before any mutation, or the build broke."
# The runner exits 1 on a survivor and on a crash alike, so its exit code says
# nothing: only a run that returned prints the report line.
nix run ".#checks.x86_64-linux.$CHECK.diff" --accept-flake-config -- \
  --base "$BASE" --no-fail-fast --out-dir "$OUT" >>"$LOG" 2>&1 || true
grep -q '^Machine-readable report: ' "$LOG" \
  || fail "the runner died before writing its report."
jq -e '.controls.failed == 0' "$OUT/report.json" >/dev/null \
  || fail "a control failed: a test is flaky or too slow, so the kills cannot be trusted."

jq --arg head "$(git rev-parse HEAD)" --arg base "$merge_base" \
  --arg notAssessed "$(not_assessed "$merge_base")" \
  -f nix/mutation-diff.jq "$OUT/report.json" >"$OUT/mutation-diff.json"
jq -j -f nix/mutation-comment.jq "$OUT/mutation-diff.json" >"$OUT/mutation-diff.md"
cat "$OUT/mutation-diff.md"
echo
echo "$OUT/mutation-diff.json"
