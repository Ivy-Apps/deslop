# report.json -> mutation-diff.json: what a reader acts on, without the
# covering_tests that make up most of a report. `schema` is ours, so a sydtest
# bump changes this filter and no consumer. The report itself is unversioned.
def one_line: gsub("[\u0000-\u0008\u000a-\u001f\u007f\u0085  ]"; " ");
def entry: {
    outcome,
    file: (.mutation.source_file | one_line),
    line: .mutation.line,
    end_line: .mutation.end_line,
    operator: (.mutation.operator | one_line),
    binding: ((.mutation.binding // "") | one_line),
    original: (.mutation.source_lines | map(one_line)),
    mutated: (.mutation.mutated_lines | map(one_line))
  };
def outcome_rank: {survived: 0, uncovered: 1, timed_out: 2, skipped: 3};

[.groups[][] | select(.mutation.operator != "Control")] as $all
| {
    schema: 1,
    summary: {
      selected: ($all | length),
      killed: .mutations.killed,
      timed_out: .mutations.timed_out,
      survived: .mutations.survived,
      # The report's own uncovered total counts Control mutants too.
      uncovered: ($all | map(select(.outcome == "uncovered")) | length),
      skipped: .mutations.skipped,
      controls_failed: .controls.failed
    },
    mutants: ($all
      | map(select(outcome_rank[.outcome] != null) | entry)
      | sort_by(outcome_rank[.outcome], .file, .line))
  }
