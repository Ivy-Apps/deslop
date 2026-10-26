"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const crypto = require("node:crypto");
const fc = require("fast-check");
const { load } = require("./load");

const { DeslopSha256 } = load();

function oracle(text) { return crypto.createHash("sha256").update(text, "utf8").digest("hex"); }

test("sha256 of the empty string is the published digest", () => {
  assert.equal(DeslopSha256.sha256(""), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
});

test("sha256 agrees with node:crypto on any unicode string, across block boundaries", () => {
  fc.assert(fc.property(fc.string({ unit: "binary", maxLength: 300, size: "large" }), (text) => {
    assert.equal(DeslopSha256.sha256(text), oracle(text));
  }));
});
