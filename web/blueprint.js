// Blueprint visualization: turn the calculator's "🛠 操作步骤" string into a
// render model for a left→right schematic of the power-splitting layout.
//
// This module is the pure core of the feature (round 4). It has no DOM
// dependency, so test/blueprint-smoke.mjs can assert the model under Node; the
// sprite tiles and DOM renderer that consume the model are added separately.
//
// Step-string semantics (from ENDBatteryCore/Calculator.swift `allActions`):
//   - Each token is `<ports><action>[×<count>]`, tokens joined by whitespace.
//   - <ports> is 2 or 3: a splitter using 2 or 3 ports. Physically every
//     splitter is the same 3-way 分流器 sprite; the number is a port badge.
//   - <action> 🟢 = add  → a branch that is merged back via a 三合一汇流器.
//             🔴 = discard → a 阻流 branch that is blocked / discarded.
//   - ×<count> is run-length encoding of consecutive identical steps.
// The string is already normalized (leading 阻流 section first, groups sorted),
// so rendering tokens left→right in print order is the "逻辑示意" layout — no
// physical belt order is implied (data-source option A).

// Sprite base names under web/assets/icons/<name>.svg (hand-drawn schematic
// vectors). The DOM renderer builds the paths from here by appending ".svg".
export const SPRITES = {
	thermalPool: "thermal-pool", // 1 热能池 — the source
	splitter: "splitter",        // 2 三分分流器 — one per step (2/3 via badge)
	merger: "merger",            // 3 三合一汇流器 — entry merger + one per 🟢 step
	beltStraight: "belt-straight", // 4 直传送带 — linear connector
	beltCurve: "belt-curve",       // 5 转弯传送带 — for folded layouts (later)
	bridge: "bridge",              // 6 物流桥 — belt crossover (later)
};

// One token of the step string, before ×count expansion.
const TOKEN_RE = /^([23])(🔴|🟢)(?:×(\d+))?$/u;

// Parse a single token like "3🔴×2" → {ports, action, count, text} or null.
function parseToken(text) {
	const m = TOKEN_RE.exec(text);
	if (!m) return null;
	return {
		ports: Number(m[1]),
		action: m[2] === "🟢" ? "add" : "discard",
		count: m[3] ? Number(m[3]) : 1,
		text,
	};
}

// Pull the actions out of a full stdout block. Returns {count, actions} for the
// "🛠 操作步骤(N):　<tokens>" line, or null if absent. `count` is the printed N
// (total step count) and `actions` is the raw token string.
export function extractStepsLine(stdout) {
	if (typeof stdout !== "string") return null;
	// The colon is followed by a fullwidth space (U+3000) in the printed line.
	const m = /🛠 操作步骤\((\d+)\)[:：]　?(.*)/u.exec(stdout);
	if (!m) return null;
	return { count: Number(m[1]), actions: m[2].trim() };
}

// Parse the actions string into a render model for the linear schematic.
//
// Returns {raw, steps, nodes, stepCount, unparsed}:
//   - steps:   flattened per-step list [{ports, action}], ×count expanded so
//              there is one entry (and one splitter sprite) per physical step.
//   - nodes:   ordered left→right render nodes. Always starts with the thermal
//              pool source and an entry merger, then a splitter per step; every
//              `add` step is followed by a merger that joins its branch back.
//   - unparsed: tokens that did not match the grammar (surfaced, not dropped).
export function parseSteps(actions) {
	const raw = typeof actions === "string" ? actions.trim() : "";

	const steps = [];
	const unparsed = [];
	if (raw !== "") {
		for (const text of raw.split(/\s+/u)) {
			const tok = parseToken(text);
			if (!tok) {
				unparsed.push(text);
				continue;
			}
			for (let i = 0; i < tok.count; i++) {
				steps.push({ ports: tok.ports, action: tok.action });
			}
		}
	}

	const nodes = [];
	if (steps.length > 0) {
		nodes.push({
			kind: "thermalPool",
			sprite: SPRITES.thermalPool,
			label: "热能池",
			role: "source",
		});
		nodes.push({
			kind: "merger",
			sprite: SPRITES.merger,
			label: "入口汇流",
			role: "entry",
		});
		for (const step of steps) {
			nodes.push({
				kind: "splitter",
				sprite: SPRITES.splitter,
				badge: step.ports, // "2" or "3" shown on the sprite
				ports: step.ports,
				action: step.action, // drives red/green tint
				label: step.action === "add" ? `${step.ports}口分流` : `${step.ports}口阻流`,
				role: "split",
			});
			if (step.action === "add") {
				nodes.push({
					kind: "merger",
					sprite: SPRITES.merger,
					label: "汇流",
					role: "merge",
				});
			}
		}
	}

	return { raw, steps, nodes, stepCount: steps.length, unparsed };
}
