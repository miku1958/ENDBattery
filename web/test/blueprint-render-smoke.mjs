// Node structural test for the blueprint DOM renderer (round 4, subtask #3).
// The renderer needs `document`, so this supplies a minimal stub and asserts the
// model→DOM mapping (tile/belt counts, port badges, add/discard tint classes,
// sprite src paths) for the two seed scenarios. The pixel/visual review of the
// rendered page is the user's job; this only locks the structural contract.
//
// Run:  node web/test/blueprint-render-smoke.mjs

// —— minimal document stub —— only the surface renderBlueprint touches.
function makeEl(tag) {
	return {
		tagName: tag,
		className: "",
		src: "",
		alt: "",
		_text: "",
		children: [],
		get textContent() {
			return this._text;
		},
		set textContent(v) {
			this._text = v;
		},
		append(...kids) {
			this.children.push(...kids);
		},
		replaceChildren(...kids) {
			this.children = [...kids];
		},
	};
}
globalThis.document = { createElement: makeEl };

const { parseSteps, renderBlueprint, SPRITES } = await import("../blueprint.js");

let failures = 0;
function check(name, cond, detail = "") {
	if (cond) {
		console.log(`  ✅ ${name}`);
	} else {
		failures++;
		console.log(`  ❌ ${name}${detail ? "\n" + detail : ""}`);
	}
}

const hasClass = (el, c) => el.className.split(/\s+/u).includes(c);
const ICON_RE = /^assets\/icons\/[a-z-]+\.svg$/u;

// The two seeds' actual "操作步骤" token strings (from the golden baseline).
const cases = [
	{ name: "4号谷地", actions: "3🔴×2     2🟢     3🟢     2🔴     2🟢     3🔴     3🟢" },
	{ name: "武陵", actions: "3🔴×2     2🔴     2🟢     3🔴×3     3🟢×2" },
];

for (const c of cases) {
	console.log(`scenario: ${c.name}`);
	const model = parseSteps(c.actions);
	const container = makeEl("div");
	renderBlueprint(model, container);

	check("container holds one chain", container.children.length === 1 && hasClass(container.children[0], "bp-chain"));
	const chain = container.children[0];

	const tiles = chain.children.filter((el) => hasClass(el, "bp-tile"));
	const belts = chain.children.filter((el) => hasClass(el, "bp-belt"));

	// One tile per node, a belt between every adjacent pair, alternating.
	check("tile count === nodes", tiles.length === model.nodes.length, `    tiles=${tiles.length} nodes=${model.nodes.length}`);
	check("belt count === nodes - 1", belts.length === model.nodes.length - 1, `    belts=${belts.length}`);
	check(
		"chain children alternate tile/belt",
		chain.children.length === 2 * model.nodes.length - 1 &&
			chain.children.every((el, i) => hasClass(el, i % 2 === 0 ? "bp-tile" : "bp-belt")),
	);

	// Front of the chain: thermal pool source, then entry merger.
	check("first tile is thermalPool", hasClass(tiles[0], "bp-thermalPool"));
	check("second tile is merger", hasClass(tiles[1], "bp-merger"));

	// Splitters: one per step, each tinted by action and badged with its port count.
	const splitterTiles = tiles.filter((el) => hasClass(el, "bp-splitter"));
	const splitterNodes = model.nodes.filter((n) => n.kind === "splitter");
	check("splitter tile count === step count", splitterTiles.length === model.stepCount, `    got ${splitterTiles.length}`);
	check(
		"splitter tint matches add/discard",
		splitterTiles.every((el, i) => hasClass(el, splitterNodes[i].action === "add" ? "bp-add" : "bp-discard")),
	);
	check(
		"splitter badge shows its 2/3 port count",
		splitterTiles.every((el, i) => {
			const icon = el.children.find((ch) => ch.className === "bp-icon");
			const badge = icon.children.find((ch) => ch.className === "bp-badge");
			return badge && badge.textContent === String(splitterNodes[i].ports);
		}),
	);

	// Every tile's img points at a real assets/icons/*.svg path.
	const imgs = [];
	for (const el of chain.children) {
		const icon = el.children.find((ch) => ch.className === "bp-icon");
		if (icon) imgs.push(icon.children.find((ch) => ch.tagName === "img"));
		else imgs.push(el.children.find((ch) => ch.tagName === "img")); // belt
	}
	check("all sprites use assets/icons/*.svg", imgs.every((img) => img && ICON_RE.test(img.src)), `    srcs=${JSON.stringify(imgs.map((i) => i && i.src))}`);
	check("belts use the straight-belt sprite", belts.every((el) => el.children[0].src === `assets/icons/${SPRITES.beltStraight}.svg`));
}

// Empty model → placeholder, not a chain, not a crash.
console.log("unit: empty + unparsed");
const emptyContainer = makeEl("div");
renderBlueprint(parseSteps(""), emptyContainer);
check("empty model → bp-empty placeholder", emptyContainer.children.length === 1 && hasClass(emptyContainer.children[0], "bp-empty"));
renderBlueprint(null, makeEl("div")); // null model must not throw
check("null model handled without throw", true);

// Unparsed tokens are surfaced as a warning, not dropped silently.
const warnContainer = makeEl("div");
renderBlueprint(parseSteps("2🟢 garbage 3🔴"), warnContainer);
const warn = warnContainer.children.find((el) => hasClass(el, "bp-warn"));
check("unparsed tokens surface a bp-warn", warn != null && warn.textContent.includes("garbage"), `    warn=${warn && warn.textContent}`);

console.log(failures === 0 ? "\nALL PASSED" : `\n${failures} CHECK(S) FAILED`);
process.exit(failures === 0 ? 0 : 1);
