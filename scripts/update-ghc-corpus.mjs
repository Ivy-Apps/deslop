// Records what GHC says each module of each case in the ghc corpus depends on.
//
// The cases are authored by hand in fixtures/haskell/ghc-corpus.json - which
// files exist and what they hold. This script materialises each one in a temp
// directory, asks `ghc -M` for the dependencies between its files, and writes
// the answer back into the `ghc` field. Nothing here decides what is correct;
// the compiler does.
//
// Run through `just update-ghc-corpus`, which needs ghc and node on PATH. The
// default test suite reads the recorded answers and never runs this, so it
// stays fast, hermetic and needs no compiler.

import { execFileSync } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, realpathSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

const corpusPath = "fixtures/haskell/ghc-corpus.json";
const corpus = JSON.parse(readFileSync(corpusPath, "utf8"));

// `ghc -M` writes one `target : dependency` line per dependency, between two
// marker comments. An object depends on the interface of each module it
// imports - `.hi`, or `.hi-boot` for a {-# SOURCE #-} import - and on its own
// source, which is no dependency between two files.
const DEPENDENCY = /^(\S+)\.o : (\S+)\.(hi|hi-boot)$/;

const dependenciesOf = (testCase) => {
    const root = realpathSync(mkdtempSync(join(tmpdir(), "deslop-ghc-corpus-")));
    try {
        for (const [path, contents] of Object.entries(testCase.files)) {
            const full = join(root, path);
            mkdirSync(dirname(full), { recursive: true });
            writeFileSync(full, contents);
        }
        const sources = Object.keys(testCase.files).filter((path) => path.endsWith(".hs"));
        execFileSync("ghc", ["-M", "-dep-makefile", "deps.mk", "-isrc", ...sources], { cwd: root, stdio: "inherit" });
        const pairs = readFileSync(join(root, "deps.mk"), "utf8")
            .split("\n")
            .map((line) => DEPENDENCY.exec(line.trim()))
            .filter((match) => match !== null)
            .map((match) => [`${match[1]}.hs`, `${match[2]}.hs`])
            // A module's dependency on its own boot file is the compiler's
            // bookkeeping, not an import.
            .filter(([from, to]) => from !== to);
        return pairs.sort((a, b) => a.join("\0").localeCompare(b.join("\0")));
    } finally {
        rmSync(root, { recursive: true, force: true });
    }
};

for (const testCase of corpus.cases) {
    testCase.ghc = dependenciesOf(testCase);
    console.log(`${testCase.ghc.map(([from, to]) => `${from} -> ${to}`).join(", ") || "(none)"}   [${testCase.name}]`);
}

writeFileSync(corpusPath, JSON.stringify(corpus, null, 2) + "\n");
