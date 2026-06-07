// Page logic for the interactive ENDBattery calculator.
//
// The form is assembled into the stdin JSON that the WASM module reads (schema
// in .github/skills/swift-wasm-web/SKILL.md step 4), run through the WASI loader,
// and its stdout is shown in the result area. Named configs live in localStorage.
//
// The pure helpers (buildStdinJson, SEED_CONFIGS, defaults) are exported and the
// DOM wiring is guarded behind `typeof document`, so test/page-config-smoke.mjs
// can drive the exact form-to-JSON path under Node — the same split loader.js
// uses to share one module between the browser and the Node smoke test.

import { runWasm } from "./loader.js";

// Battery kinds, matching ENDBatteryCore's Config.Battery and its Chinese names.
export const BATTERY_TYPES = [
	{ value: "originium", label: "源石" },
	{ value: "green", label: "绿电池" },
	{ value: "blue", label: "蓝电池" },
	{ value: "purple", label: "紫电池" },
	{ value: "lowEarth", label: "低容息壤电池" },
	{ value: "midEarth", label: "中容息壤电池" },
];

// Top-level tunables. Defaults mirror CalculatorInput.init(from:) so that a form
// left untouched reproduces the same output as omitting the keys entirely.
export const OPTION_SPEC = [
	{ key: "minAnalyzedBatteryCount", label: "最少分流电池数", type: "int", default: 1 },
	{ key: "extraBeltInSteps", label: "额外传送带步数", type: "int", default: 1 },
	{ key: "maxDepthLimit", label: "最大递归深度", type: "int", default: 9 },
	{ key: "showTopSolutions", label: "输出方案数", type: "int", default: 1 },
	{ key: "allowedMinDiff", label: "允许最小差值", type: "number", default: 10 },
	{ key: "enableThree", label: "允许三分流", type: "bool", default: true },
	{ key: "safetyThreshold", label: "安全阈值", type: "number", default: 0.15 },
	{ key: "maxStopToOutageSeconds", label: "停流到停电上限 (秒, 留空=自动)", type: "nullableNumber", default: null },
	{ key: "maxShortageDurationLimitInSecond", label: "最长缺电时间 (秒)", type: "number", default: 1000 },
	{ key: "keepAllSolutions", label: "保留所有方案 (很慢)", type: "bool", default: false },
];

export const OPTION_DEFAULTS = Object.fromEntries(
	OPTION_SPEC.map((o) => [o.key, o.default]),
);

// Seed scenarios shown on first load — the same two the Swift tests assert on,
// so the page is immediately usable and reproduces the golden baseline.
export const SEED_CONFIGS = [
	{
		name: "4号谷地",
		analyzedType: "purple",
		baseRequiredPower: 5230,
		staticBattery: [{ type: "purple", count: "" }],
		options: {},
	},
	{
		name: "武陵",
		analyzedType: "purple",
		baseRequiredPower: 6210,
		staticBattery: [{ type: "midEarth", count: 1 }],
		options: {},
	},
];

// Turn a config object into the stdin JSON string. `configs` is always length 1:
// the page computes one scenario per run.
export function buildStdinJson(config) {
	const staticBattery = (config.staticBattery || [])
		.filter((b) => b && b.type)
		.map((b) => {
			const entry = { type: b.type };
			if (b.count !== null && b.count !== undefined && b.count !== "") {
				entry.count = Number(b.count);
			}
			return entry;
		});

	const opts = { ...OPTION_DEFAULTS, ...(config.options || {}) };

	const input = {
		configs: [
			{
				name: config.name,
				staticBattery,
				analyzedBattery: { type: config.analyzedType },
				baseRequiredPower: Number(config.baseRequiredPower),
			},
		],
		...opts,
	};

	return JSON.stringify(input);
}

// ————————————————————————————— DOM wiring —————————————————————————————

const STORAGE_KEY = "endbattery.configs.v1";

function blankConfig() {
	return {
		name: "新配置",
		analyzedType: "purple",
		baseRequiredPower: "",
		staticBattery: [],
		options: {},
	};
}

function initPage() {
	const $ = (id) => document.getElementById(id);

	const els = {
		select: $("config-select"),
		btnNew: $("btn-new"),
		btnSave: $("btn-save"),
		btnDelete: $("btn-delete"),
		form: $("calc-form"),
		name: $("f-name"),
		analyzed: $("f-analyzed"),
		power: $("f-power"),
		staticRows: $("static-rows"),
		btnAddStatic: $("btn-add-static"),
		optionsGrid: $("options-grid"),
		status: $("status"),
		result: $("result"),
		stderr: $("stderr"),
	};

	const setStatus = (msg) => {
		els.status.textContent = msg;
	};

	// Compile the wasm module once, lazily, and reuse it across runs (each run
	// instantiates a fresh instance, so module-level state never leaks).
	let wasmModulePromise = null;
	function getWasmModule() {
		if (!wasmModulePromise) {
			wasmModulePromise = compileWasm().catch((e) => {
				wasmModulePromise = null; // allow retry on next calculate
				throw e;
			});
		}
		return wasmModulePromise;
	}
	async function compileWasm() {
		try {
			return await WebAssembly.compileStreaming(fetch("ENDBattery.wasm"));
		} catch {
			// Fallback for servers that don't send `application/wasm`.
			const resp = await fetch("ENDBattery.wasm");
			if (!resp.ok) throw new Error(`无法下载 ENDBattery.wasm (HTTP ${resp.status})`);
			return WebAssembly.compile(await resp.arrayBuffer());
		}
	}

	// Build a <select> of battery types with the given selected value.
	function batterySelect(selected) {
		const sel = document.createElement("select");
		for (const t of BATTERY_TYPES) {
			const opt = document.createElement("option");
			opt.value = t.value;
			opt.textContent = `${t.label} (${t.value})`;
			if (t.value === selected) opt.selected = true;
			sel.append(opt);
		}
		return sel;
	}

	// Populate the analyzed-battery dropdown once.
	for (const t of BATTERY_TYPES) {
		const opt = document.createElement("option");
		opt.value = t.value;
		opt.textContent = `${t.label} (${t.value})`;
		els.analyzed.append(opt);
	}

	// One static-battery row: type select + optional count + remove button.
	function addStaticRow(entry = { type: "purple", count: "" }) {
		const row = document.createElement("div");
		row.className = "static-row";

		const sel = batterySelect(entry.type);
		sel.dataset.role = "type";

		const count = document.createElement("input");
		count.type = "number";
		count.min = "0";
		count.step = "1";
		count.placeholder = "数量 (可空)";
		count.dataset.role = "count";
		count.value = entry.count == null ? "" : String(entry.count);

		const remove = document.createElement("button");
		remove.type = "button";
		remove.textContent = "删除";
		remove.addEventListener("click", () => row.remove());

		row.append(sel, count, remove);
		els.staticRows.append(row);
	}

	function collectStaticBattery() {
		return [...els.staticRows.querySelectorAll(".static-row")].map((row) => ({
			type: row.querySelector('[data-role="type"]').value,
			count: row.querySelector('[data-role="count"]').value,
		}));
	}

	// Render the advanced-options inputs from OPTION_SPEC (single source of truth).
	for (const spec of OPTION_SPEC) {
		const wrap = document.createElement("label");
		wrap.className = "option";

		const text = document.createElement("span");
		text.textContent = spec.label;

		let input;
		if (spec.type === "bool") {
			input = document.createElement("input");
			input.type = "checkbox";
		} else {
			input = document.createElement("input");
			input.type = "number";
			input.step = spec.type === "int" ? "1" : "any";
		}
		input.dataset.optionKey = spec.key;
		input.dataset.optionType = spec.type;

		wrap.append(text, input);
		els.optionsGrid.append(wrap);
	}

	function setOptionInputs(options) {
		const merged = { ...OPTION_DEFAULTS, ...(options || {}) };
		for (const input of els.optionsGrid.querySelectorAll("[data-option-key]")) {
			const key = input.dataset.optionKey;
			const value = merged[key];
			if (input.dataset.optionType === "bool") {
				input.checked = Boolean(value);
			} else {
				input.value = value == null ? "" : String(value);
			}
		}
	}

	function collectOptions() {
		const options = {};
		for (const input of els.optionsGrid.querySelectorAll("[data-option-key]")) {
			const key = input.dataset.optionKey;
			const type = input.dataset.optionType;
			if (type === "bool") {
				options[key] = input.checked;
			} else if (input.value === "") {
				options[key] = type === "nullableNumber" ? null : OPTION_DEFAULTS[key];
			} else if (type === "int") {
				options[key] = parseInt(input.value, 10);
			} else {
				options[key] = parseFloat(input.value);
			}
		}
		return options;
	}

	function fillForm(config) {
		els.name.value = config.name ?? "";
		els.analyzed.value = config.analyzedType ?? "purple";
		els.power.value = config.baseRequiredPower == null ? "" : String(config.baseRequiredPower);
		els.staticRows.replaceChildren();
		for (const entry of config.staticBattery || []) addStaticRow(entry);
		setOptionInputs(config.options);
	}

	function collectForm() {
		return {
			name: els.name.value.trim(),
			analyzedType: els.analyzed.value,
			baseRequiredPower: els.power.value,
			staticBattery: collectStaticBattery(),
			options: collectOptions(),
		};
	}

	// ——— stored configs ———

	let configs = loadStoredConfigs();
	if (!configs || configs.length === 0) {
		configs = SEED_CONFIGS.map((c) => structuredClone(c));
		persistConfigs(configs);
	}

	function loadStoredConfigs() {
		try {
			const raw = localStorage.getItem(STORAGE_KEY);
			if (!raw) return null;
			const parsed = JSON.parse(raw);
			return Array.isArray(parsed) ? parsed : null;
		} catch {
			return null;
		}
	}

	function persistConfigs(list) {
		try {
			localStorage.setItem(STORAGE_KEY, JSON.stringify(list));
		} catch (e) {
			setStatus(`无法写入 localStorage: ${e.message}`);
		}
	}

	function refreshSelect(selectedName) {
		els.select.replaceChildren();
		for (const c of configs) {
			const opt = document.createElement("option");
			opt.value = c.name;
			opt.textContent = c.name;
			if (c.name === selectedName) opt.selected = true;
			els.select.append(opt);
		}
	}

	// ——— event handlers ———

	els.select.addEventListener("change", () => {
		const c = configs.find((c) => c.name === els.select.value);
		if (c) fillForm(c);
	});

	els.btnNew.addEventListener("click", () => {
		fillForm(blankConfig());
		els.name.focus();
		els.name.select();
		setStatus("新建配置 — 编辑后点「保存」存入浏览器");
	});

	els.btnSave.addEventListener("click", () => {
		const c = collectForm();
		if (!c.name) {
			setStatus("请先填写配置名称");
			return;
		}
		const idx = configs.findIndex((x) => x.name === c.name);
		if (idx >= 0) configs[idx] = c;
		else configs.push(c);
		persistConfigs(configs);
		refreshSelect(c.name);
		setStatus(`已保存「${c.name}」`);
	});

	els.btnDelete.addEventListener("click", () => {
		const name = els.select.value;
		configs = configs.filter((c) => c.name !== name);
		persistConfigs(configs);
		if (configs.length === 0) {
			refreshSelect();
			fillForm(blankConfig());
		} else {
			refreshSelect(configs[0].name);
			fillForm(configs[0]);
		}
		setStatus(`已删除「${name}」`);
	});

	els.btnAddStatic.addEventListener("click", () => addStaticRow());

	els.form.addEventListener("submit", async (e) => {
		e.preventDefault();
		const json = buildStdinJson(collectForm());
		els.result.textContent = "";
		els.stderr.textContent = "";
		setStatus("计算中…");
		try {
			const mod = await getWasmModule();
			const { stdout, stderr, exitCode } = await runWasm(mod, json);
			els.result.textContent = stdout || "(无输出)";
			els.stderr.textContent = stderr;
			setStatus(exitCode === 0 ? "完成" : `进程退出码 ${exitCode}`);
		} catch (err) {
			setStatus(`出错: ${err.message}`);
		}
	});

	// ——— first paint ———

	refreshSelect(configs[0].name);
	fillForm(configs[0]);
	getWasmModule().then(
		() => setStatus("就绪 — 填好参数点「开始计算」"),
		(e) => setStatus(`加载 wasm 失败: ${e.message}`),
	);
	setStatus("正在加载 wasm…");
}

if (typeof document !== "undefined") {
	if (document.readyState === "loading") {
		document.addEventListener("DOMContentLoaded", initPage);
	} else {
		initPage();
	}
}
