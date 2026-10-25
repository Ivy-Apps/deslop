# The diagram explorer's assets

`deslop diagram` writes one self-contained page: `explorer.html` with the
scripts below and the graph inlined into it at compile time
(`src/Deslop/Diagram/Html.hs`). Edit `explorer.html` freely; the vendored
scripts are copied verbatim from their npm tarballs and never edited.

| File | Package | Version | License |
| --- | --- | --- | --- |
| `cytoscape.min.js` | [cytoscape](https://www.npmjs.com/package/cytoscape) (`dist/cytoscape.min.js`) | 3.34.4 | MIT, `LICENSE-cytoscape` |
| `elk.bundled.js` | [elkjs](https://www.npmjs.com/package/elkjs) (`lib/elk.bundled.js`) | 0.9.3 | EPL-2.0, `LICENSE-elkjs.md`; source at https://github.com/kieler/elkjs |
| `cytoscape-elk.js` | [cytoscape-elk](https://www.npmjs.com/package/cytoscape-elk) (`dist/cytoscape-elk.js`) | 2.3.0 | MIT, `LICENSE-cytoscape-elk` |

elkjs is pinned to 0.9.3, the line cytoscape-elk 2.3.0 declares (`^0.9.3`).

To update one, download its tarball from the registry and copy the same file:

```bash
curl -sL https://registry.npmjs.org/elkjs/-/elkjs-0.9.3.tgz | tar xz package/lib/elk.bundled.js
```
