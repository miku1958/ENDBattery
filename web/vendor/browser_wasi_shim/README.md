# Vendored: @bjorn3/browser_wasi_shim

Pinned copy of the `dist/` ESM build of [`@bjorn3/browser_wasi_shim`](https://github.com/bjorn3/browser_wasi_shim).

- Version: **0.4.2**
- License: MIT OR Apache-2.0 (see `LICENSE-MIT`)

It is vendored (not pulled from a CDN) so the GitHub Pages site is fully
self-contained: no runtime third-party fetch, no JS build step, identical
module in the browser and in the Node smoke test.

To update: `npm pack @bjorn3/browser_wasi_shim@<ver>`, replace the `*.js`
files here with the new `dist/*.js`, and bump the version above.
