// Node smoke test for the page's form-to-JSON path. app.js guards its DOM wiring
// behind `typeof document`, so importing it under Node yields only the pure
// helpers. We feed each seed config through buildStdinJson — the exact JSON the
// page sends when that config is selected and "开始计算" is clicked — run it
// through the same WASI loader, and assert the golden lines the Swift tests use.
//
// Requires a local cross-compiled wasm build (not committed):
//   . ~/.swiftly/env.sh && hash -r
//   swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm -j 8
//
// Run:  node test/page-config-smoke.mjs

import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

import { runWasm } from "../loader.js";
import { buildStdinJson, SEED_CONFIGS } from "../app.js";

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = join(here, "..");
const wasmPath = join(repoRoot, ".build/wasm32-unknown-wasip1/release/ENDBattery.wasm");

let failures = 0;
function check(name, cond, detail = "") {
	if (cond) {
		console.log(`  ✅ ${name}`);
	} else {
		failures++;
		console.log(`  ❌ ${name}${detail ? "\n" + detail : ""}`);
	}
}

const wasmModule = await WebAssembly.compile(await readFile(wasmPath));

// The golden lines per seed scenario — same values the Swift tests assert on.
const expected = {
	"4号谷地": [
		"📦 电池数量:　高容谷地电池(常开): 3个, 高容谷地电池(分流): 2个",
		"🔌 最终功率:　1742.7984",
		"💎 净收益:　　0.010391 颗/秒  ≈  897.778颗/天(理论可省: 922.909 颗/天, 差值: 25.131)",
	],
	"武陵": [
		"📦 电池数量:　中容武陵电池: 1个, 高容谷地电池(常开): 2个, 高容谷地电池(分流): 1个",
		"🔌 最终功率:　621.1706",
		"💎 净收益:　　0.010882 颗/秒  ≈  940.247颗/天(理论可省: 962.182 颗/天, 差值: 21.935)",
	],
};

for (const config of SEED_CONFIGS) {
	console.log(`seed config: ${config.name}`);
	const json = buildStdinJson(config);
	const { stdout, stderr, exitCode } = await runWasm(wasmModule, json);
	check("exitCode === 0", exitCode === 0, `    exitCode=${exitCode}`);
	check("stderr empty", stderr === "", `    stderr=${JSON.stringify(stderr)}`);
	check(`searched ${config.name}`, stdout.includes(`正在搜索:　${config.name}`));
	for (const line of expected[config.name]) {
		check(line, stdout.includes(line), "    stdout did not contain it");
	}
}

console.log(failures === 0 ? "\nALL PASSED" : `\n${failures} CHECK(S) FAILED`);
process.exit(failures === 0 ? 0 : 1);
