// Node unit test for the blueprint render model (round 4, pure core — no wasm,
// no DOM). Asserts parseSteps/extractStepsLine against the two seed scenarios'
// "🛠 操作步骤" lines from the golden baseline, covering the render rules the
// renderer relies on: one splitter per step, a merger per 🟢, source + entry
// merger at the front, correct 2/3 port badges, and ×N expansion.
//
// Run:  node web/test/blueprint-smoke.mjs

import { parseSteps, extractStepsLine, SPRITES } from "../blueprint.js";

let failures = 0;
function check(name, cond, detail = "") {
	if (cond) {
		console.log(`  ✅ ${name}`);
	} else {
		failures++;
		console.log(`  ❌ ${name}${detail ? "\n" + detail : ""}`);
	}
}

// The exact "操作步骤" lines from logs/baseline-current-output.txt.
const cases = [
	{
		name: "4号谷地",
		line: "\t🛠 操作步骤(8):　3🔴×2     2🟢     3🟢     2🔴     2🟢     3🔴     3🟢",
		stepCount: 8,
		addCount: 4, // 2🟢 3🟢 2🟢 3🟢
		discardCount: 4, // 3🔴×2 2🔴 3🔴
	},
	{
		name: "武陵",
		line: "\t🛠 操作步骤(9):　3🔴×2     2🔴     2🟢     3🔴×3     3🟢×2",
		stepCount: 9,
		addCount: 3, // 2🟢 3🟢×2
		discardCount: 6, // 3🔴×2 2🔴 3🔴×3
	},
];

for (const c of cases) {
	console.log(`scenario: ${c.name}`);

	const extracted = extractStepsLine(c.line);
	check("extractStepsLine found the line", extracted !== null);
	check(`printed count === ${c.stepCount}`, extracted?.count === c.stepCount, `    got ${extracted?.count}`);

	const model = parseSteps(extracted.actions);

	check("no unparsed tokens", model.unparsed.length === 0, `    unparsed=${JSON.stringify(model.unparsed)}`);

	// "🛠(N)" printed count must equal the expanded step count.
	check(`stepCount === printed N (${c.stepCount})`, model.stepCount === c.stepCount, `    got ${model.stepCount}`);

	const splitters = model.nodes.filter((n) => n.kind === "splitter");
	const mergers = model.nodes.filter((n) => n.kind === "merger");
	const merges = mergers.filter((n) => n.role === "merge");

	// One splitter sprite per step.
	check("splitter count === stepCount", splitters.length === model.stepCount, `    got ${splitters.length}`);
	// Each 🟢 (add) gets exactly one merger after it.
	check(`merge count === add count (${c.addCount})`, merges.length === c.addCount, `    got ${merges.length}`);
	check(`add steps === ${c.addCount}`, model.steps.filter((s) => s.action === "add").length === c.addCount);
	check(`discard steps === ${c.discardCount}`, model.steps.filter((s) => s.action === "discard").length === c.discardCount);

	// Source + entry merger are at the very front.
	check("nodes[0] is thermalPool source", model.nodes[0]?.kind === "thermalPool" && model.nodes[0]?.role === "source");
	check("nodes[1] is entry merger", model.nodes[1]?.kind === "merger" && model.nodes[1]?.role === "entry");

	// Every splitter carries a 2 or 3 badge matching its sprite.
	check("all splitter ports are 2 or 3", splitters.every((n) => n.ports === 2 || n.ports === 3));
	check("splitter sprite is the splitter tile", splitters.every((n) => n.sprite === SPRITES.splitter));
}

// ×N expansion: "3🔴×2" must yield two discard splitter nodes.
console.log("unit: ×N expansion");
const rle = parseSteps("3🔴×2");
check("3🔴×2 → 2 steps", rle.stepCount === 2, `    got ${rle.stepCount}`);
check("both are 3-port discards", rle.steps.every((s) => s.ports === 3 && s.action === "discard"));

// Empty / missing input yields an empty model, not a crash.
console.log("unit: empty + malformed input");
const empty = parseSteps("");
check("empty actions → 0 steps, 0 nodes", empty.stepCount === 0 && empty.nodes.length === 0);
check("undefined actions → 0 steps", parseSteps(undefined).stepCount === 0);
check("extractStepsLine(no match) → null", extractStepsLine("no steps here") === null);
const bad = parseSteps("3🔴 9🟢 garbage 2🟢");
check("malformed tokens go to unparsed (not dropped silently)", bad.unparsed.length === 2 && bad.stepCount === 2, `    unparsed=${JSON.stringify(bad.unparsed)} steps=${bad.stepCount}`);

console.log(failures === 0 ? "\nALL PASSED" : `\n${failures} CHECK(S) FAILED`);
process.exit(failures === 0 ? 0 : 1);
