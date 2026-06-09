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

// ————————————————————————————— DOM renderer —————————————————————————————
//
// Render a parseSteps() model into `container` as a left→right flex chain of
// sprite tiles, with a straight-belt sprite between every pair of tiles.
// Splitter tiles carry a 2/3 port badge and a green (add) / red (discard) tint;
// the transparent SVG sprites let the tint show through.
//
// `document` is only touched when this is called, so the pure-core smoke test
// (which imports parseSteps/extractStepsLine/SPRITES) loads the module fine
// under Node; the render smoke test supplies a minimal document stub instead.

// Sprite paths are built relative to the page, matching app.js's fetch("ENDBattery.wasm").
const ICON_DIR = "assets/icons";

function spriteTile(node) {
	const tile = document.createElement("div");
	const cls = ["bp-tile", `bp-${node.kind}`];
	if (node.kind === "splitter") cls.push(node.action === "add" ? "bp-add" : "bp-discard");
	tile.className = cls.join(" ");

	const icon = document.createElement("div");
	icon.className = "bp-icon";

	const img = document.createElement("img");
	img.src = `${ICON_DIR}/${node.sprite}.svg`;
	img.alt = node.label;
	icon.append(img);

	if (node.badge != null) {
		const badge = document.createElement("span");
		badge.className = "bp-badge";
		badge.textContent = String(node.badge);
		icon.append(badge);
	}
	tile.append(icon);

	const label = document.createElement("div");
	label.className = "bp-label";
	label.textContent = node.label;
	tile.append(label);

	return tile;
}

function beltTile() {
	const belt = document.createElement("div");
	belt.className = "bp-belt";
	const img = document.createElement("img");
	img.src = `${ICON_DIR}/${SPRITES.beltStraight}.svg`;
	img.alt = "传送带";
	belt.append(img);
	return belt;
}

// Render `model` (from parseSteps) into `container`, replacing its contents.
// An empty model shows a placeholder; unparsed tokens are surfaced, not dropped.
export function renderBlueprint(model, container) {
	container.replaceChildren();

	if (!model || model.nodes.length === 0) {
		const empty = document.createElement("p");
		empty.className = "bp-empty";
		empty.textContent = "(无分流步骤)";
		container.append(empty);
		return;
	}

	const chain = document.createElement("div");
	chain.className = "bp-chain";
	model.nodes.forEach((node, i) => {
		if (i > 0) chain.append(beltTile());
		chain.append(spriteTile(node));
	});
	container.append(chain);

	if (model.unparsed.length > 0) {
		const warn = document.createElement("p");
		warn.className = "bp-warn";
		warn.textContent = `未识别步骤: ${model.unparsed.join(" ")}`;
		container.append(warn);
	}
}
