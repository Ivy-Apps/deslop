// Records what `tsc` resolves each case in the resolution corpus to.
//
// The cases are authored by hand in fixtures/resolution-corpus.json - what
// files exist, what the tsconfig says, and which specifier is written where.
// This script materialises each one in a temp directory, asks the real
// compiler, and writes its answer back into the `tsc` field. Nothing here
// decides what is correct; the compiler does.
//
// Run through `just update-resolution-corpus`, which puts node and tsc on
// PATH. The default test suite reads the recorded answers and never runs this,
// so it stays fast, hermetic and needs no node.

import { execFileSync } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, realpathSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";

const corpusPath = "fixtures/resolution-corpus.json";
const corpus = JSON.parse(readFileSync(corpusPath, "utf8"));

// `tsc` prints one of these per module it tries to resolve.
const RESOLVED = /^======== Module name '(.+?)' was successfully resolved to '(.+?)'/;
const UNRESOLVED = /^======== Module name '(.+?)' was not resolved/;

const resolveCase = (testCase) => {
    // realpath, because /tmp is a symlink on macOS and tsc reports the real
    // path: without it every answer would come back as '../../private/tmp/...'.
    const root = realpathSync(mkdtempSync(join(tmpdir(), "deslop-corpus-")));
    try {
        // Written verbatim: the corpus holds each config's literal contents, so
        // that what tsc was asked is exactly what the spec feeds Deslop.
        for (const [path, config] of Object.entries(testCase.configs)) {
            write(root, path, JSON.stringify(config, null, 2));
        }
        for (const path of testCase.files) {
            const body = path === testCase.from ? `import "${testCase.specifier}";\n` : "export {};\n";
            write(root, path, body);
        }

        const trace = runTsc(root);
        const line = trace.find((l) => {
            const match = RESOLVED.exec(l) ?? UNRESOLVED.exec(l);
            return match !== null && match[1] === testCase.specifier;
        });
        if (line === undefined) {
            throw new Error(`tsc never tried to resolve '${testCase.specifier}' in case '${testCase.name}'`);
        }

        const resolved = RESOLVED.exec(line);
        // Relative to the case root, and always '/'-spelled, so the recorded
        // answer names no temp directory and no machine.
        return resolved === null ? null : relative(root, resolve(resolved[2])).split(/[\\/]/).join("/");
    } finally {
        rmSync(root, { recursive: true, force: true });
    }
};

const write = (root, path, contents) => {
    const full = join(root, path);
    mkdirSync(dirname(full), { recursive: true });
    writeFileSync(full, contents);
};

// tsc exits non-zero on a type error, which several cases have on purpose:
// an unresolved import is the answer being recorded, not a failure.
const runTsc = (root) => {
    try {
        return execFileSync("tsc", ["--noEmit", "--traceResolution"], {
            cwd: root,
            encoding: "utf8",
        }).split("\n");
    } catch (err) {
        if (err.stdout === undefined) throw err;
        return err.stdout.split("\n");
    }
};

for (const testCase of corpus.cases) {
    testCase.tsc = resolveCase(testCase);
    console.log(`${testCase.tsc ?? "(unresolved)"}  <-  ${testCase.specifier}   [${testCase.name}]`);
}

writeFileSync(corpusPath, JSON.stringify(corpus, null, 2) + "\n");
console.log(`\nRecorded ${corpus.cases.length} cases into ${corpusPath}`);
