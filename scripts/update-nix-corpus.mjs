// Records what Nix itself says the path literals of each case in the nix
// corpus are.
//
// The cases are authored by hand in fixtures/nix/nix-corpus.json - the source of
// one file each. This script writes each one to a temp directory, asks
// `nix-instantiate --parse` for its syntax tree and writes the path literals
// it holds back into the `nix` field. Nothing here decides what is correct;
// Nix's parser does.
//
// Run through `just update-nix-corpus`, which needs nix-instantiate and node on
// PATH. The default test suite reads the recorded answers and never runs this,
// so it stays fast, hermetic and needs no Nix.

import { spawnSync } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, realpathSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const corpusPath = "fixtures/nix/nix-corpus.json";
const corpus = JSON.parse(readFileSync(corpusPath, "utf8"));

// Where the corpus says each case lives. The temp directory is spelled this
// way in the answers, so they do not depend on the machine that recorded them.
const CORPUS_ROOT = "/corpus";

// `--parse` prints the expression back as Nix, normalised: comments are gone,
// every string is double-quoted - an interpolated one becomes a `+` between its
// parts, a literal `${` is escaped, and only a dynamic attribute name keeps a
// `${ }` - and an operator such as `/` becomes a call, `__div`. So in code, a
// `/` not part of `//` starts a path literal, which Nix has already made
// absolute: the text before its first interpolation is the path, the rest is
// strings concatenated to it.
const PATH_CHAR = /[a-zA-Z0-9._+\-\/]/;

// A search path is printed as the lookup it stands for, which an application
// it is the head of is printed flat after.
const SEARCH_PATH = /^__findFile __nixPath "([^"\\]*)"/;

const literalsOf = (printed) => {
    const paths = [];
    const searchPaths = [];
    // Scans code from `i` to the `}` closing it, or to the end, and returns
    // the index after it.
    const code = (i) => {
        let depth = 0;
        while (i < printed.length) {
            const searchPath = SEARCH_PATH.exec(printed.slice(i));
            if (searchPath !== null) {
                searchPaths.push(searchPath[1]);
                i += searchPath[0].length;
            } else if (printed[i] === '"') {
                i = string(i + 1);
            } else if (printed.startsWith("//", i)) {
                i += 2;
            } else if (printed[i] === "/") {
                let end = i + 1;
                while (end < printed.length && PATH_CHAR.test(printed[end])) end++;
                paths.push(printed.slice(i, end));
                i = end;
            } else if (printed[i] === "{") {
                depth++;
                i++;
            } else if (printed[i] === "}") {
                if (depth === 0) return i + 1;
                depth--;
                i++;
            } else {
                i++;
            }
        }
        return i;
    };
    // Scans a string from after its opening quote and returns the index after
    // its closing one.
    const string = (i) => {
        while (printed[i] !== '"') {
            if (printed[i] === "\\") i += 2;
            else if (printed.startsWith("${", i)) i = code(i + 2);
            else i++;
        }
        return i + 1;
    };
    code(0);
    return { paths: paths.sort(), searchPaths: searchPaths.sort() };
};

const verdictOf = (testCase) => {
    const root = realpathSync(mkdtempSync(join(tmpdir(), "deslop-nix-corpus-")));
    try {
        const project = join(root, "project");
        const home = join(root, "home");
        mkdirSync(project);
        mkdirSync(home);
        writeFileSync(join(project, "case.nix"), testCase.source);
        const run = spawnSync("nix-instantiate", ["--parse", "case.nix"], {
            cwd: project,
            env: { ...process.env, HOME: home },
            encoding: "utf8",
        });
        if (run.error) throw run.error;
        const fromCorpus = (text) => text.replaceAll(root, CORPUS_ROOT);
        if (run.status === 0) return { parses: literalsOf(fromCorpus(run.stdout)) };
        const error = fromCorpus(run.stderr).split("\n").find((line) => line.startsWith("error: "));
        if (error === undefined) throw new Error(`nix-instantiate failed without an error on [${testCase.name}]:\n${run.stderr}`);
        return { rejects: error.slice("error: ".length) };
    } finally {
        rmSync(root, { recursive: true, force: true });
    }
};

for (const testCase of corpus.cases) {
    testCase.nix = verdictOf(testCase);
    const { parses, rejects } = testCase.nix;
    const summary = parses ? [...parses.paths, ...parses.searchPaths.map((s) => `<${s}>`)].join(", ") || "(none)" : `rejects: ${rejects}`;
    console.log(`${summary}   [${testCase.name}]`);
}

writeFileSync(corpusPath, JSON.stringify(corpus, null, 2) + "\n");
