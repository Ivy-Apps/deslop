// Where the camera goes for a wheel or a Safari pinch, the way canvas tools like Figma move it. Pure: it touches no DOM.
// A Viewport is cytoscape's { zoom, pan: { x, y } }; points are rendered pixels inside the canvas.
var DeslopNavigation = (function () {
  "use strict";

  // How many pixels a line of a line-based wheel (deltaMode 1) moves, as Firefox scrolls a page.
  var LINE_PIXELS = 16;
  // How much one pixel of pinch zooms by: e^(-deltaY * ZOOM_RATE).
  var ZOOM_RATE = 0.01;
  // A Ctrl + mouse wheel notch is ~100px, a pinch a few: capped, a notch zooms by e^0.5 rather than e^1.
  var MAX_ZOOM_STEP = 50;

  // A wheel's deltas in pixels, whatever unit it came in. A page is the canvas's size.
  function pixels(wheel, page) {
    var units = [{ x: 1, y: 1 }, { x: LINE_PIXELS, y: LINE_PIXELS }, { x: page.width, y: page.height }];
    var unit = units[wheel.deltaMode] || units[0];
    return { x: wheel.deltaX * unit.x, y: wheel.deltaY * unit.y };
  }

  // A pinch (ctrlKey, as browsers report it) or Ctrl/Cmd + wheel zooms around the cursor; any other wheel pans, Shift turning a vertical wheel sideways.
  function onWheel(viewport, wheel, cursor, page, limits) {
    var d = pixels(wheel, page);
    if (wheel.ctrlKey || wheel.metaKey) {
      var step = Math.max(-MAX_ZOOM_STEP, Math.min(MAX_ZOOM_STEP, d.y));
      return zoomAround(viewport, viewport.zoom * Math.exp(-step * ZOOM_RATE), cursor, limits);
    }
    var sideways = wheel.shiftKey && d.x === 0;
    return panBy(viewport, sideways ? d.y : d.x, sideways ? 0 : d.y);
  }

  // Safari's pinch: its scale is relative to the zoom the gesture started at.
  function onGesture(viewport, startZoom, scale, cursor, limits) {
    return zoomAround(viewport, startZoom * scale, cursor, limits);
  }

  // The content moves against the fingers, as a page scrolls.
  function panBy(viewport, dx, dy) {
    return { zoom: viewport.zoom, pan: { x: viewport.pan.x - dx, y: viewport.pan.y - dy } };
  }

  // Zoomed to `level`, clamped to the limits, keeping the graph point under `point` where it is on screen.
  function zoomAround(viewport, level, point, limits) {
    var zoom = Math.max(limits.min, Math.min(limits.max, level));
    var ratio = zoom / viewport.zoom;
    return { zoom: zoom, pan: { x: point.x - (point.x - viewport.pan.x) * ratio, y: point.y - (point.y - viewport.pan.y) * ratio } };
  }

  return { onWheel: onWheel, onGesture: onGesture };
})();
