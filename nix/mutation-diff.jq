# report.json -> mutation-diff.json, schema 1 of the mutation diff contract our
# repositories share: what a reader acts on, without the covering_tests that
# make up most of a report. `schema` is ours, so a sydtest bump changes this
# filter and no consumer. The report itself is unversioned.
#
# Arguments: $head and $base, full shas, and $notAssessed, a sentence or "".
def one_line: gsub("[\u0000-\u0008\u000a-\u001f\u007f\u0085  ]"; " ");
# The source lines, not the tool's own original and replacement: those name a
# sub-expression, often a lone `e`, which says nothing without its line.
def lines: map(one_line) | join("\n");
def status: {survived: "survived", uncovered: "uncovered", timed_out: "timedOut"}[.outcome];
def status_rank: {survived: 0, uncovered: 1, timedOut: 2}[.];
def entry: {
    file: (.mutation.source_file | one_line),
    line: .mutation.line,
    status: status,
    operator: (.mutation.operator | one_line),
    original: (.mutation.source_lines | lines),
    replacement: (.mutation
      # ListLit, RemoveClause and RemoveAction carry no mutated lines, only this.
      | if .mutated_lines == [] then "(\(.original) -> \(.replacement))" | one_line
        else .mutated_lines | lines end)
  };

[.groups[][] | select(.mutation.operator != "Control")] as $all
| {
    schema: 1,
    tool: "sydtest-mutation",
    head: $head,
    base: $base,
    totals: {
      mutants: ($all | length),
      killed: .mutations.killed,
      survived: .mutations.survived,
      timedOut: .mutations.timed_out,
      # The report's own uncovered total counts Control mutants too.
      uncovered: ($all | map(select(.outcome == "uncovered")) | length),
      skipped: .mutations.skipped
    },
    mutants: ($all
      | map(select(status != null) | entry)
      | sort_by((.status | status_rank), .file, .line)),
    notAssessed: $notAssessed
  }
