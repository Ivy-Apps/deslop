// The lowercase hex SHA-256 of a string's UTF-8 bytes. Synchronous and self-contained:
// crypto.subtle exists only in secure contexts, and the page is also served over plain http.
var DeslopSha256 = (function () {
  "use strict";

  // The first 32 bits of the fractional parts of the cube roots of the first 64 primes.
  var K = [];
  (function () {
    var found = 0;
    for (var c = 2; found < 64; c++) {
      var prime = true;
      for (var d = 2; d * d <= c; d++) if (c % d === 0) { prime = false; break; }
      if (prime) K[found++] = (Math.pow(c, 1 / 3) % 1) * 4294967296 | 0;
    }
  })();

  function sha256(text) {
    var bytes = Array.from(new TextEncoder().encode(text));
    var bitLength = bytes.length * 8;
    bytes.push(0x80);
    while (bytes.length % 64 !== 56) bytes.push(0);
    for (var shift = 56; shift >= 0; shift -= 8) bytes.push(shift >= 32 ? Math.floor(bitLength / Math.pow(2, shift)) & 0xff : (bitLength >>> shift) & 0xff);
    var h = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19];
    var w = new Array(64);
    function rotr(x, n) { return (x >>> n) | (x << (32 - n)); }
    for (var i = 0; i < bytes.length; i += 64) {
      for (var t = 0; t < 64; t++) {
        if (t < 16) w[t] = (bytes[i + 4 * t] << 24) | (bytes[i + 4 * t + 1] << 16) | (bytes[i + 4 * t + 2] << 8) | bytes[i + 4 * t + 3];
        else {
          var s0 = rotr(w[t - 15], 7) ^ rotr(w[t - 15], 18) ^ (w[t - 15] >>> 3);
          var s1 = rotr(w[t - 2], 17) ^ rotr(w[t - 2], 19) ^ (w[t - 2] >>> 10);
          w[t] = (w[t - 16] + s0 + w[t - 7] + s1) | 0;
        }
      }
      var a = h[0], b = h[1], c = h[2], d = h[3], e = h[4], f = h[5], g = h[6], k = h[7];
      for (t = 0; t < 64; t++) {
        var t1 = (k + (rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)) + ((e & f) ^ (~e & g)) + K[t] + w[t]) | 0;
        var t2 = ((rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)) + ((a & b) ^ (a & c) ^ (b & c))) | 0;
        k = g; g = f; f = e; e = (d + t1) | 0; d = c; c = b; b = a; a = (t1 + t2) | 0;
      }
      h = [h[0] + a, h[1] + b, h[2] + c, h[3] + d, h[4] + e, h[5] + f, h[6] + g, h[7] + k].map(function (x) { return x | 0; });
    }
    return h.map(function (x) { return ("00000000" + (x >>> 0).toString(16)).slice(-8); }).join("");
  }

  return { sha256: sha256 };
})();
