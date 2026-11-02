# mutation-diff.json -> mutation-diff.md: the report `just mutation-diff` prints
# and the pull request comment carries. Every PR-derived string sits in one
# fence, and every line in it starts with @, - or +, so no line can close the
# fence. mutation-diff.jq already stripped control characters, which a bare CR
# would otherwise use to escape it. notAssessed is ours, written from counts.
def budget_bytes: 58000; # GitHub refuses a comment over 65536 characters.
def side(prefix): split("\n")[:12] | map(prefix + .[:200]) | join("\n");
def block: "@@ \(.status) \(.file):\(.line) \(.operator) @@\n\(.original | side("- "))\n\(.replacement | side("+ "))\n";

.totals as $t
| reduce (.mutants[] | block) as $b ({text: "", shown: 0};
    if (.text + $b | utf8bytelength) > budget_bytes then . else .text += $b | .shown += 1 end) as $r
| ((.mutants | length) - $r.shown) as $cut
| (if $t.mutants == 0 then
     "No mutants in this diff, so nothing was tested. Only added or changed `src/` lines select mutants.\n"
   else
     "| Mutants | Killed | Survived | Uncovered | Timed out | Skipped |\n|---|---|---|---|---|---|\n| \($t.mutants) | \($t.killed) | \($t.survived) | \($t.uncovered) | \($t.timedOut) | \($t.skipped) |\n"
   end)
  + (if $r.shown == 0 then "" else "\n```diff\n" + $r.text + "```\n" end)
  + (if $cut > 0 then "\n\($cut) more not shown: they are in `mutation-diff.json`.\n" else "" end)
  + (if .notAssessed == "" then "" else "\nNot assessed: \(.notAssessed)\n" end)
