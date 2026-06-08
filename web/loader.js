// Run the ENDBattery WASM module under a WASI shim: feed it a JSON string on
// stdin, run it to completion, and return the captured stdout/stderr.
//
// The browser page and the Node smoke test (test/loader-smoke.mjs) both import
// this module, so it must stay free of any browser- or Node-only API beyond
// WebAssembly + Text{Encoder,Decoder}.

import { WASI, File, OpenFile, ConsoleStdout } from "./vendor/browser_wasi_shim/index.js";

// Join the captured byte chunks into one Uint8Array.
function concatChunks(chunks) {
	let total = 0;
	for (const c of chunks) total += c.byteLength;
	const out = new Uint8Array(total);
	let offset = 0;
	for (const c of chunks) {
		out.set(c, offset);
		offset += c.byteLength;
	}
	return out;
}

// Run a compiled `WebAssembly.Module` with `stdinJson` on stdin.
// Returns { stdout, stderr, exitCode } with stdout/stderr decoded as UTF-8.
export async function runWasm(wasmModule, stdinJson) {
	const stdinBytes = new TextEncoder().encode(stdinJson);

	const stdoutChunks = [];
	const stderrChunks = [];

	// fd 0/1/2. ConsoleStdout hands each fd_write its own copied slice, so the
	// chunks stay valid after the call; collect raw bytes and decode once at the
	// end to keep multi-byte UTF-8 boundaries intact.
	const fds = [
		new OpenFile(new File(stdinBytes, { readonly: true })),
		new ConsoleStdout((bytes) => stdoutChunks.push(bytes)),
		new ConsoleStdout((bytes) => stderrChunks.push(bytes)),
	];

	// Pass { debug: false } explicitly: the shim treats an absent debug option as
	// "enabled" and would otherwise spam console.log on every WASI call.
	const wasi = new WASI(["ENDBattery"], [], fds, { debug: false });

	// A compiled Module (not bytes) makes WebAssembly.instantiate resolve to the
	// Instance directly.
	const instance = await WebAssembly.instantiate(wasmModule, {
		wasi_snapshot_preview1: wasi.wasiImport,
	});

	const exitCode = wasi.start(instance);

	const decoder = new TextDecoder("utf-8");
	return {
		stdout: decoder.decode(concatChunks(stdoutChunks)),
		stderr: decoder.decode(concatChunks(stderrChunks)),
		exitCode,
	};
}
