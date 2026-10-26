"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const fc = require("fast-check");
const { load, plain } = require("./load");

const { DeslopNavigation } = load();

const LIMITS = { min: 0.05, max: 4 };
const PAGE = { width: 800, height: 600 };
const CURSOR = { x: 300, y: 200 };
const AT_ONE = { zoom: 1, pan: { x: 0, y: 0 } };

function wheel(fields) { return Object.assign({ deltaX: 0, deltaY: 0, deltaMode: 0, ctrlKey: false, metaKey: false, shiftKey: false }, fields); }
function onWheel(viewport, fields) { return plain(DeslopNavigation.onWheel(viewport, wheel(fields), CURSOR, PAGE, LIMITS)); }
// Where a graph point is drawn on screen.
function rendered(viewport, p) { return { x: p.x * viewport.zoom + viewport.pan.x, y: p.y * viewport.zoom + viewport.pan.y }; }
function close(a, b) { return Math.abs(a - b) <= 1e-6 * Math.max(1, Math.abs(a), Math.abs(b)); }

const anyViewport = fc.record({
  zoom: fc.double({ min: LIMITS.min, max: LIMITS.max, noNaN: true }),
  pan: fc.record({ x: fc.double({ min: -1e4, max: 1e4, noNaN: true }), y: fc.double({ min: -1e4, max: 1e4, noNaN: true }) })
});
const anyPoint = fc.record({ x: fc.double({ min: 0, max: 2000, noNaN: true }), y: fc.double({ min: 0, max: 2000, noNaN: true }) });
const anyDelta = fc.double({ min: -500, max: 500, noNaN: true });

test("onWheel pans a two-finger move against the fingers, as a page scrolls", () => {
  assert.deepEqual(onWheel(AT_ONE, { deltaX: 10, deltaY: -20 }), { zoom: 1, pan: { x: -10, y: 20 } });
});

test("onWheel reads a line-based wheel's lines as pixels", () => {
  assert.deepEqual(onWheel(AT_ONE, { deltaY: 3, deltaMode: 1 }), { zoom: 1, pan: { x: 0, y: -48 } });
});

test("onWheel reads a page-based wheel's pages as the canvas's size", () => {
  assert.deepEqual(onWheel(AT_ONE, { deltaX: 1, deltaY: 1, deltaMode: 2 }), { zoom: 1, pan: { x: -800, y: -600 } });
});

test("onWheel turns Shift + a vertical wheel sideways", () => {
  assert.deepEqual(onWheel(AT_ONE, { deltaY: 30, shiftKey: true }), { zoom: 1, pan: { x: -30, y: 0 } });
});

test("onWheel leaves Shift + a sideways wheel as it is", () => {
  assert.deepEqual(onWheel(AT_ONE, { deltaX: 30, shiftKey: true }), { zoom: 1, pan: { x: -30, y: 0 } });
});

test("onWheel zooms in on a pinch out", () => {
  assert.ok(onWheel(AT_ONE, { deltaY: -5, ctrlKey: true }).zoom > 1);
});

test("onWheel zooms out on a pinch in", () => {
  assert.ok(onWheel(AT_ONE, { deltaY: 5, ctrlKey: true }).zoom < 1);
});

test("onWheel zooms on Cmd + wheel as on Ctrl + wheel", () => {
  assert.deepEqual(onWheel(AT_ONE, { deltaY: -5, metaKey: true }), onWheel(AT_ONE, { deltaY: -5, ctrlKey: true }));
});

test("onWheel zooms a Ctrl + mouse wheel notch by a bounded step", () => {
  const notch = onWheel(AT_ONE, { deltaY: -100, ctrlKey: true });
  const huge = onWheel(AT_ONE, { deltaY: -10000, ctrlKey: true });

  assert.equal(huge.zoom, notch.zoom);
  assert.ok(notch.zoom < 2);
});

test("onWheel keeps the zoom within the limits, however far it pinches", () => {
  fc.assert(fc.property(anyViewport, anyDelta, fc.integer({ min: 0, max: 2 }), (viewport, deltaY, deltaMode) => {
    const { zoom } = onWheel(viewport, { deltaY, deltaMode, ctrlKey: true });

    assert.ok(zoom >= LIMITS.min && zoom <= LIMITS.max);
  }));
});

test("onWheel zooms around the cursor: the graph point under it stays under it", () => {
  fc.assert(fc.property(anyViewport, anyDelta, anyPoint, (viewport, deltaY, graphPoint) => {
    const cursor = rendered(viewport, graphPoint);
    const after = DeslopNavigation.onWheel(viewport, wheel({ deltaY, ctrlKey: true }), cursor, PAGE, LIMITS);
    const moved = rendered(after, graphPoint);

    assert.ok(close(moved.x, cursor.x) && close(moved.y, cursor.y), JSON.stringify({ cursor, moved }));
  }));
});

test("onGesture zooms to the scale of the zoom the pinch started at, around the cursor", () => {
  const after = plain(DeslopNavigation.onGesture({ zoom: 2, pan: { x: 0, y: 0 } }, 1, 4, CURSOR, LIMITS));

  assert.deepEqual(after, { zoom: 4, pan: { x: -300, y: -200 } });
});

test("onGesture clamps a pinch past the limits", () => {
  assert.equal(DeslopNavigation.onGesture(AT_ONE, 1, 100, CURSOR, LIMITS).zoom, LIMITS.max);
  assert.equal(DeslopNavigation.onGesture(AT_ONE, 1, 0.001, CURSOR, LIMITS).zoom, LIMITS.min);
});
