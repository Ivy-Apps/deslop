# ts-gitignore-project

Fixture for `.gitignore` support. It carries **real** `.gitignore` files at three
depths so that `Git.Ignore` is exercised end to end:

| file                     | verdict | what it pins                                     |
| ------------------------ | ------- | ------------------------------------------------ |
| `src/app.ts`             | linted  | ordinary file                                    |
| `src/bad.ts`             | linted  | proves the run happened at all                   |
| `src/keep.gen.ts`        | linted  | later line wins within one file (`!src/keep.gen.ts`) |
| `src/other.gen.ts`       | linted  | deeper `.gitignore` overrides shallower          |
| `src/features/sub/scratch.ts` | linted | anchored `/scratch.ts` does not reach here    |
| `lib/helper.gen.ts`      | ignored | `*.gen.ts` matches a basename at any depth       |
| `generated/model.ts`     | ignored | anchored, directory-only `/generated/`           |
| `generated/nested/deep.ts` | ignored | an ignored directory is pruned, not descended  |
| `vendor/legacy.ts`       | ignored | bare `vendor` matches a directory at any depth   |
| `src/debug/trace.ts`     | ignored | `debug/` is scoped to `src/`, not the root       |
| `src/features/scratch.ts` | ignored | anchored to `src/features`                      |

These verdicts were checked against real `git check-ignore`.

## Adding an ignored file here

The `.gitignore` files in this fixture are honoured by **this repository's** git
too, so anything they ignore is untracked and will not be committed by an
ordinary `git add`. Force it:

```sh
git add -f test/fixtures/ts-gitignore-project/<path>
```

Once tracked, git leaves the file alone, so this is a one-time step per file.
