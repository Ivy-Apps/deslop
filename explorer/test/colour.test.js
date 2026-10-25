"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const { load } = require("./load");

const { DeslopColour } = load();

test("mix blends two colours channel by channel, by the weight of the first", () => {
  assert.equal(DeslopColour.mix("#ff0000", "#0000ff", 0.25), "#4000bf");
});
