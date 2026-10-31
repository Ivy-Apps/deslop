# mutation-diff.json -> the pull request comment's numbers and mutants. Every
# PR-derived string sits in one fence, and every line in it starts with @, - or
# +, so no line can close the fence. mutation-diff.jq already stripped control
# characters, which a bare CR would otherwise use to escape it.
def budget_bytes: 58000; # GitHub refuses a comment over 65536 characters.
def block: "@@ \(.outcome) \(.file):\(.line) \(.operator) \(.binding) @@\n"
  + ((.original[:12] | map("- " + .[:200])) + (.mutated[:12] | map("+ " + .[:200])) | join("\n")) + "\n";

.summary as $s
| reduce (.mutants[] | block) as $b ({text: "", shown: 0};
    if (.text + $b | utf8bytelength) > budget_bytes then . else .text += $b | .shown += 1 end) as $r
| ((.mutants | length) - $r.shown) as $cut
| (if $s.selected == 0 then
     "No mutants in this diff, so nothing was tested. Only added or changed `src/` lines select mutants: changes to tests alone, or a change that only deletes, are not assessed.\n"
   else
     "| Selected | Killed | Survived | Uncovered | Timed out | Skipped |\n|---|---|---|---|---|---|\n| \($s.selected) | \($s.killed) | \($s.survived) | \($s.uncovered) | \($s.timed_out) | \($s.skipped) |\n"
   end)
  + (if $r.shown == 0 then "" else "\n```diff\n" + $r.text + "```\n" end)
  + (if $cut > 0 then "\n\($cut) more not shown: they are in `mutation-diff.json`, below.\n" else "" end)
