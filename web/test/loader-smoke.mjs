// Node smoke test for loader.js — the browser path has no browser here, so we
// drive the exact same loader + vendored WASI shim under Node and compare its
// stdout against the committed golden baseline.
//
// Requires a local cross-compiled wasm build (it is not committed):
//   . ~/.swiftly/env.sh && hash -r
//   cd swift && swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm -j 8
//
// Run:  node web/test/loader-smoke.mjs

import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

import { runWasm } from "../loader.js";

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = join(here, "..", "..");
const wasmPath = join(repoRoot, "swift/.build/wasm32-unknown-wasip1/release/ENDBattery.wasm");
const baselinePath = join(repoRoot, "logs/baseline-current-output.txt");

// Drop the non-deterministic trailing "🕐 总耗时: X.XX 秒" timing line so the
// comparison is over the deterministic report body only.
function stripTiming(s) {
	const idx = s.indexOf("🕐 总耗时");
	return idx >= 0 ? s.slice(0, idx) : s;
}

let failures = 0;
function check(name, cond, detail = "") {
	if (cond) {
		console.log(`  ✅ ${name}`);
	} else {
		failures++;
		console.log(`  ❌ ${name}${detail ? "\n" + detail : ""}`);
	}
}

const wasmBytes = await readFile(wasmPath);
const wasmModule = await WebAssembly.compile(wasmBytes);

// 1. Byte-exact (minus timing) vs the golden baseline: both configs in one run.
console.log("combined run vs baseline:");
const combined = {
	configs: [
		{ name: "4号谷地", staticBattery: [{ type: "purple" }], analyzedBattery: { type: "purple" }, baseRequiredPower: 5230 },
		{ name: "武陵", staticBattery: [{ type: "midEarth", count: 1 }], analyzedBattery: { type: "purple" }, baseRequiredPower: 6210 },
	],
};
const combinedResult = await runWasm(wasmModule, JSON.stringify(combined));
check("exitCode === 0", combinedResult.exitCode === 0, `    exitCode=${combinedResult.exitCode}`);
check("stderr empty", combinedResult.stderr === "", `    stderr=${JSON.stringify(combinedResult.stderr)}`);
check("both configs searched", combinedResult.stdout.includes("正在搜索:　4号谷地") && combinedResult.stdout.includes("正在搜索:　武陵"));

// Byte-exact (minus timing) vs the golden baseline when it is present. The
// baseline lives under gitignored logs/, so a fresh checkout may lack it; skip
// loudly rather than crash, the inline line checks below still cover the values.
let baseline = null;
try {
	baseline = await readFile(baselinePath, "utf-8");
} catch {
	console.log("  ⏭  baseline file absent — skipping byte-exact comparison (run from a tree with logs/baseline-current-output.txt for it)");
}
if (baseline !== null) {
	check(
		"stdout matches baseline (timing line excluded)",
		stripTiming(combinedResult.stdout) === stripTiming(baseline),
		`    --- got ---\n${combinedResult.stdout}\n    --- want ---\n${baseline}`,
	);
}

// 2. Single-config runs reproduce the key lines the Swift tests assert on.
const cases = [
	{
		label: "4号谷地",
		json: { configs: [{ name: "4号谷地", staticBattery: [{ type: "purple" }], analyzedBattery: { type: "purple" }, baseRequiredPower: 5230 }] },
		lines: [
			"📦 电池数量:　高容谷地电池(常开): 3个, 高容谷地电池(分流): 2个",
			"🔌 最终功率:　1742.7984",
			"💎 净收益:　　0.010391 颗/秒  ≈  897.778颗/天(理论可省: 922.909 颗/天, 差值: 25.131)",
		],
	},
	{
		label: "武陵",
		json: { configs: [{ name: "武陵", staticBattery: [{ type: "midEarth", count: 1 }], analyzedBattery: { type: "purple" }, baseRequiredPower: 6210 }] },
		lines: [
			"📦 电池数量:　中容武陵电池: 1个, 高容谷地电池(常开): 2个, 高容谷地电池(分流): 1个",
			"🔌 最终功率:　621.1706",
			"💎 净收益:　　0.010882 颗/秒  ≈  940.247颗/天(理论可省: 962.182 颗/天, 差值: 21.935)",
		],
	},
];
for (const c of cases) {
	console.log(`single run: ${c.label}`);
	const { stdout } = await runWasm(wasmModule, JSON.stringify(c.json));
	for (const line of c.lines) {
		check(line, stdout.includes(line), `    stdout did not contain it`);
	}
}

console.log(failures === 0 ? "\nALL PASSED" : `\n${failures} CHECK(S) FAILED`);
process.exit(failures === 0 ? 0 : 1);
