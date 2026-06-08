# TODO — swift-wasm-web

## 本次任务

把 ENDBattery 编译成 WebAssembly,加一个**交互式**网页(HTML 表单填参数 → WASM 计算 → 显示结果),托管在 GitHub Pages。

已确认设计:
- 交互式方案(非"只跑固定配置"):网页表单提供电池类型、`baseRequiredPower` 及选填项,计算结果显示在页面上。
- **产品入口只有 HTML/WASM,不再保留产品级 CLI**。原 `Sources/ENDBattery/main.swift` “硬编码 configs 直接 print”的用途取消。
- 计算逻辑抽成可被测试 import 的 library target;WASM executable 从 **stdin JSON** 读取 → 调用核心 → print 结果。
- **本地验证与 debug 都走 `swift test`**:现在硬编码的 `4号谷地`/`武陵` 等场景迁为测试用例。
- 输入传递:**stdin 喂一段 JSON**,Swift 端解码(对应 question 选项 B)。
- 页面位置:仓库**根目录**(html/js 源文件放根目录)。
- config 存储:浏览器 **localStorage** 保存;网页 UI 支持保存 / 切换多个 config(用户可建多个命名配置并切换)。
- `.wasm` 上线:**GitHub Actions 构建后部署到 Pages**(Pages source = "GitHub Actions")。`.wasm` 不进 git,push Swift 改动后 CI 自动重编重部,网页永远是最新。

## 本地环境(已就绪,无需重装)

swiftly + 6.3.2 toolchain + 完整版 wasm SDK 已装在本机用户目录,不在仓库内。每个新 shell 用前先激活:

```bash
. ~/.swiftly/env.sh && hash -r            # swift → 6.3.2
cd swift                                  # 整个 SPM 包在 swift/
swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm -j 8   # 产物: swift/.build/wasm32-unknown-wasip1/release/ENDBattery.wasm
swift run   -c release --swift-sdk swift-6.3.2-RELEASE_wasm        # WasmKit 本地试跑
```

装法见 [SKILL.md](SKILL.md) Procedure 第 1 步。

## 待办(按执行顺序)

1. ✅ 已完成。拆出 `ENDBatteryCore` library(`Calculator.swift` + `Input.swift`)、`ENDBattery` executable(stdin JSON → `runCalculation` → print)、`ENDBatteryCoreTests`(`4号谷地`/`武陵` + 默认值场景,dup2 捕获 stdout 断言)。`swift test` 3 通过;host 与 WASM(WasmKit 喂 stdin)输出对照 `logs/baseline-current-output.txt` 字节一致。JSON schema、target 布局、WASI stdin 实测见 [SKILL.md](SKILL.md)。
2. ✅ 已完成。浏览器侧 loader [loader.js](../../../loader.js):`runWasm(wasmModule, stdinJson)` 用**已 vendor**的 `@bjorn3/browser_wasi_shim`([vendor/browser_wasi_shim](../../../vendor/browser_wasi_shim),v0.4.2,不走 CDN,浏览器与 Node 同一份)装 WASI、`OpenFile(File(stdinBytes))` 喂 fd0、两个 `ConsoleStdout` 收 fd1/fd2 原始字节、`wasi.start()` 跑、解码返回 `{stdout,stderr,exitCode}`。**坑**:`new WASI(...)` 第四参必须传 `{ debug: false }`,否则 shim 把缺省当"开"狂刷 `console.log`。验证:[test/loader-smoke.mjs](../../../test/loader-smoke.mjs) 在 Node 里用同一 loader 跑 wasm,合并两 config 的输出去掉计时行后与 [logs/baseline-current-output.txt](../../../logs/baseline-current-output.txt) 字节一致,单 config 命中 swift test 断言行,`exitCode 0`/`stderr 空`,`node test/loader-smoke.mjs` 全绿。
3. ✅ 已完成。页面 [index.html](../../../index.html) + [app.js](../../../app.js):场景表单(配置名、分流电池类型、`baseRequiredPower`、动态固定电池行)+ 高级选项(10 个 tunable,从 `app.js` 的 `OPTION_SPEC` 单一来源生成,默认值对齐 `CalculatorInput`)+ 结果 `<pre>` + localStorage(key `endbattery.configs.v1`)多 config 保存/切换/删除/新建 UI,首次载入种入 `4号谷地`/`武陵` 两个种子。提交按钮把表单拼成 stdin JSON(`buildStdinJson`,`configs` 恒长度 1)调 `runWasm` 渲染 `stdout`/`stderr`。wasm 经 `compileStreaming(fetch('ENDBattery.wasm'))`(失败回落 `compile(arrayBuffer)`,兼容不发 `application/wasm` 的服务器)惰性编一次、跨次运行复用 Module。app.js 把纯函数(`buildStdinJson`/`SEED_CONFIGS`/`OPTION_*`)导出、DOM 接线用 `typeof document` guard 包住(同 loader.js 浏览器/Node 共用思路)。验证:[test/page-config-smoke.mjs](../../../test/page-config-smoke.mjs) 在 Node 把两个种子经 `buildStdinJson` → `runWasm` 跑出与基线一致的关键行,`node test/page-config-smoke.mjs` 全绿;`node --check app.js` 过;`loader-smoke` 仍全绿。**未验证(留给 #5)**:真实浏览器里表单渲染 / localStorage 持久化 / 配置切换的交互。
4. ✅ 已完成。部署 workflow [.github/workflows/deploy-pages.yml](../../workflows/deploy-pages.yml):`build` job 跑在**官方 `swift:6.3.2` 容器**里(toolchain 预装,版本与下面装的 wasm SDK 精确匹配——比在 CI 里现装 swiftly 更可复现)→ `swift sdk install <同本地的 URL+checksum>` → `swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm --product ENDBattery` → 把 `index.html`/`app.js`/`loader.js`/`vendor/` + 产物 `.build/wasm32-unknown-wasip1/release/ENDBattery.wasm` 拷进 `_site/` → `upload-pages-artifact`;`deploy` job 跑 `configure-pages`(只读已 enable 的 Pages 站点,见 #5:默认 token 不能自举)+ `deploy-pages`。**尺寸取舍**:实测 wasm 58MB(完整 Foundation,未瘦身),但 < Pages 单文件 100MB 限,且 Pages/Fastly 对 `application/wasm` 自动 gzip/br,裸传只剩零头——首版直接发,不引入 `wasm-opt`/`-Osize`(最简实现;如后续要瘦身再在 build job 加 binaryen,属增量)。根目录 `ENDBattery.wasm` 仍 `.gitignore`,CI 每次重编重放。**离线已验证**:YAML 经 ruby psych 解析通过;assemble 步在 scratch 里 dry-run 出的 `_site` 树与页面 `fetch("ENDBattery.wasm")` / `import vendor/.../index.js` / `src="app.js"` 路径逐一对应;build 命令、wasm 产物路径、`swift:6.3.2` 镜像存在性均已核对。**无法离线验证(留给 #5)**:真实 Actions run(容器内 node 注入、CI 里 `swift sdk install`、upload/deploy-pages、Pages enable)。
5. ✅ 已完成。端到端验证 + 上线确认。**根因**:首个 run([27096818494](https://github.com/miku1958/ENDBattery/actions/runs/27096818494))build job 绿、deploy job 在 Configure Pages 失败(HTTP 404 "Get Pages site failed")—— Pages 站点未创建,而 `configure-pages@v5` 的 `enablement` 输入按其 action.yml **明确不接受默认 `GITHUB_TOKEN`**,故无法自举。**修复**:① 用 owner gh token 一次性开启 Pages `gh api -X POST repos/miku1958/ENDBattery/pages -f build_type=workflow`(返回 `build_type: workflow`, `html_url: https://miku1958.github.io/ENDBattery/`);② 改正 workflow 头部误导注释(commit `9ae893a`)。**重跑绿**:run [27096952533](https://github.com/miku1958/ENDBattery/actions/runs/27096952533) build ✓48s + deploy ✓9s。**线上验证**:`index.html`/`app.js`/`loader.js`/`vendor/.../index.js` 均 200;`ENDBattery.wasm` 200 + `application/wasm`(60496913 B);下载线上 `.wasm` 经 `loader.js` 跑 `武陵` 种子 → `621.1706` + 正确电池数、exitCode 0,与 `test武陵` 一致。**回归门全绿**:`swift test` 3/3、`node test/loader-smoke.mjs`、`node test/page-config-smoke.mjs`。证据 [logs/08-pages-deploy-verify.log](../../../logs/08-pages-deploy-verify.log)(gitignored)。

## 待确认(需用户输入,非 agent 可自决)

- **真实浏览器交互验证**:表单渲染(含第二轮 #1 标签/下拉文案、#4「分流电池」分组)/ localStorage 持久化 / 多 config 切换-保存-删除 这些**纯 DOM 交互**尚未在真实 Chrome 里实测过(纯数据/计算路径已被 `page-config-smoke` + 线上 wasm 实测覆盖)。要实测需用 `chrome-cdp` 驱动真实浏览器,而该能力须用户**显式批准**后才可用。问题与 Node 20 弃用提醒一并记入 `question.md`。
- **README 已陈旧**:[README.md](../../../README.md) 仍描述 round 1 已删除的产品级 CLI(`swift run` 跑硬编码 `Config.myBase`、`swiftc Sources/ENDBattery/main.swift`、旧输出格式 `所需电池数量:`、旧电池名表),与现 HTML/WASM 产品完全不符。属 round 1 删 CLI 时漏改的整篇陈旧文档,非第二轮改名范围;如何处理(按现产品重写 / 删除)需用户决定,故未在本轮顺手改。

## 第二轮:UI 改进需求(2026-06-08,已完成)

用户反馈 5 点,均已落地。验证门全绿:`swift test` 3/3、`node test/loader-smoke.mjs`、`node test/page-config-smoke.mjs`(后两者对照重生成的 [logs/baseline-current-output.txt](../../../logs/baseline-current-output.txt) byte-exact)、`node --check app.js`;wasm 已用新名重编(`.build/wasm32-unknown-wasip1/release/ENDBattery.wasm`)。编号沿用用户原话。

1. ✅ **移除界面英文标识符**。[index.html](../../../index.html):`基础需求功率 (baseRequiredPower)` → `基础需求功率`、legend `固定电池 (staticBattery)` → `固定电池`。[app.js](../../../app.js):两处电池下拉文案 `${t.label} (${t.value})` → `t.label`(去掉 `(green)`/`(purple)` 等英文 value)。
2. ✅ **标签文字选中即聚焦** 随 #1 自然消除:移除标识符后无复制需求,故不拆描述文本与 input(保持 `<label>` 包裹 `<input>` 的标准行为)。
3. ✅ **电池改游戏正式名称**(源石不改)。改 [Calculator.swift](../../../Sources/ENDBatteryCore/Calculator.swift) `Battery.name`(单一来源,驱动结果文本)+ [app.js](../../../app.js) `BATTERY_TYPES` label;同步 `ENDBatteryCoreTests` 断言、两个 node smoke 断言、重生成 baseline、重编 wasm。改名仅改显示串,所有数值不变(smoke byte-exact 已证)。映射(功率/寿命常量与游戏实测吻合,确证非推断;来源:游民星空各容量电池蓝图/效率分析、巴哈姆特 PWM 发电攻略):

   | code | 改为 | 功率/寿命 |
   |---|---|---|
   | `green` | 低容谷地电池 | 220 / 40s |
   | `blue` | 中容谷地电池 | 420 / 40s |
   | `purple` | 高容谷地电池 | 1100 / 40s |
   | `lowEarth` | 低容武陵电池 | 1600 / 40s |
   | `midEarth` | 中容武陵电池 | 3200 / 40s |
   | `originium` | 源石(不改) | 50 / 8s |

4. ✅ **表单重排**。[index.html](../../../index.html) 把「分流电池类型」从「场景」拆出,新建「分流电池」fieldset 置于「固定电池」之后。最终结构:场景(配置名称 + 基础需求功率)→ 固定电池 → 分流电池 → 高级选项。`f-analyzed` id 不变,app.js 接线无需改。
5. ✅ **本地预览 cp 工作流** 已记录在 [SKILL.md](SKILL.md) Validation 段(产物 gitignored,起静态服务器前 `cp swift/.build/wasm32-unknown-wasip1/release/ENDBattery.wasm web/`;改过 Swift 电池名要先重编再 cp)。无代码改动。

## 第三轮:目录重组 web/ + swift/(2026-06-08 用户确认,已完成)

用户经 AskUserQuestion 确认布局:前端全进 `web/`,整个 SPM 包(Package.swift + Sources + Tests)进 `swift/`,根目录只剩 `web/ swift/ .github/ logs/ README.md CLAUDE.md LICENSE .gitignore .vscode/`。**用户要求执行**;下面是已执行的 turnkey 清单。

**已执行(2026-06-08)**:`git mv` 把前端 → `web/`、SPM 包 → `swift/`(保历史)。改完 `web/test/*.mjs`(`repoRoot` 上两级 + wasm 路径 `swift/.build/...`)、`deploy-pages.yml`(两 swift 步加 `working-directory: swift` + 组装 cp 改 `web/*` / `swift/.build/*`)、`.gitignore`(`/ENDBattery.wasm` → `/web/ENDBattery.wasm`,删冗余 `/.build`)、`CLAUDE.md` / `copilot-instructions.md` / `SKILL.md` / `README.md` 路径段。**验证门全绿**:`cd swift && swift test` 3/3;`node web/test/loader-smoke.mjs`、`node web/test/page-config-smoke.mjs` 对照 [logs/baseline-current-output.txt](../../../logs/baseline-current-output.txt) byte-exact;本地 `cd web && python3 -m http.server` curl:`/app.js` / `/loader.js` / `/vendor/.../index.js` / `/index.html` 均 200、`/ENDBattery.wasm` 200 + `application/wasm`(60497191 B)、index.html 无残留英文标识符;workflow YAML 经 ruby psych 解析通过、组装路径与页面 `fetch`/`import` 逐一对应。

### 1. 文件搬移(`git mv` 保历史;移动文件实体属就地编辑规则的允许例外)

- `mkdir web && git mv index.html app.js loader.js vendor test web/`
- `mkdir swift && git mv Package.swift Sources Tests swift/`
- 不动:`.github/ logs/ README.md CLAUDE.md LICENSE .gitignore .vscode/`。`.swiftpm/`、`_site/`、`battery_opt`、根 `ENDBattery.wasm` 均 gitignored/未跟踪(无需 `git mv`);Xcode 会在 `swift/` 重新生成 `.swiftpm`。
- Package.swift 用隐式路径(`Sources/<target>`),与 Sources/Tests 一起平移后相对结构不变,**Package.swift 不用改**。

### 2. 相对路径:整体平移即不破,只 `web/test/*.mjs` 要改

- `web/app.js` `import "./loader.js"`、`web/loader.js` `import "./vendor/..."`、`web/index.html` `<script src="app.js">` 与 `fetch("ENDBattery.wasm")`:全相对、随文件同移,**不改**。
- `web/test/*.mjs` 的 `import "../app.js"` / `"../loader.js"`:`../` 仍指 `web/`,**不改**。
- `web/test/loader-smoke.mjs` 与 `web/test/page-config-smoke.mjs` **各改两处**:`repoRoot = join(here, "..")` → `join(here, "..", "..")`(现 `web/test` 上两级才是仓库根);`wasmPath` 的 `.build/...` → `swift/.build/wasm32-unknown-wasip1/release/ENDBattery.wasm`。`baselinePath` 的 `logs/...` 字面不变(repoRoot 修对后自然指对)。

### 3. CI workflow [.github/workflows/deploy-pages.yml](../../workflows/deploy-pages.yml)

- `swift sdk install` 与 `swift build` 两步加 `working-directory: swift`。
- wasm cp 源 → `swift/.build/wasm32-unknown-wasip1/release/ENDBattery.wasm`。
- 组装 `_site`:`cp web/index.html web/app.js web/loader.js _site/`、`cp -R web/vendor _site/vendor`、wasm cp 到 `_site/ENDBattery.wasm`。`_site` 内 index.html 与 wasm 仍同级,**线上行为不变**。

### 4. .gitignore

- `/ENDBattery.wasm` → `/web/ENDBattery.wasm`(本地 cp 目标移到 `web/`)。
- `.build/`(无前导斜杠)已匹配 `swift/.build/`,无需改;锚根的 `/.build` 失效,顺手删那一行。

### 5. 文档路径(grep 后逐处改)

- [CLAUDE.md](../../../CLAUDE.md):`Sources/ENDBattery/main.swift` → `swift/Sources/ENDBattery/main.swift`,及构建/验证段。
- [.github/copilot-instructions.md](../../copilot-instructions.md):WASM entry 路径同上。
- 本 [SKILL.md](SKILL.md):`swift build/test` 改在 `swift/` 跑、wasm 路径 `swift/.build/...`、本地 cp 命令 `cp swift/.build/.../ENDBattery.wasm web/`。
- 本 [TODO.md](TODO.md):第二轮 #5 与「本地环境」段的构建/ cp 路径同步。
- grep `README.md` 看有无路径引用,有则改。

### 6. 本地预览(改后)

```bash
. ~/.swiftly/env.sh && hash -r
cd swift && nice -n 19 swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm -j 8 && cd ..
cp swift/.build/wasm32-unknown-wasip1/release/ENDBattery.wasm web/ENDBattery.wasm
cd web && python3 -m http.server 8000      # 开 http://localhost:8000/
```

### 7. 验证门(全绿才算完)

- `cd swift && swift test`(host)3/3。
- `node web/test/loader-smoke.mjs`、`node web/test/page-config-smoke.mjs` 对照 baseline byte-exact。
- 本地起 server `curl`:`/app.js` 新名、`/ENDBattery.wasm` 200 + `application/wasm`、`/index.html` 无残留英文标识符。
- YAML 解析 workflow;`_site` 组装路径与页面 `fetch`/`import` 逐一对应。

## 第四轮:网页「分流蓝图」可视化(2026-06-08 用户确认,进行中)

经 AskUserQuestion 确认:产出 = **网页可视化示意图**(用从游戏截图提取的 6 个 sprite 渲染);范围 = **通用功能**(任意算法步骤输出 → 自动渲染),集成进现有页面;保真度 = **逻辑示意**,不要求游戏内可导入(故无需逆向游戏蓝图格式 / 精确接线)。

### 子任务进度(按执行顺序)

1. ✅ **步骤串解析器 + 渲染模型(纯核心,Node 可测)**。[web/blueprint.js](../../../web/blueprint.js):`parseSteps(actions)` 把 `🛠 操作步骤` 串解析成左→右线性示意的渲染模型,`extractStepsLine(stdout)` 从整段 stdout 抽出 `操作步骤(N):　<tokens>` 行。token 文法 `^([23])(🔴|🟢)(?:×(\d+))?$`:`2/3`=分流口数(同一 sprite,数字作 badge)、`🟢`=add(分流后接汇流器)、`🔴`=discard(阻流,不汇流)、`×N`=游程展开成 N 个独立步。模型 `nodes` = `[热能池(source) → 入口汇流(entry merger) → 每步 splitter(add 步再跟一个 merge merger)]`;`unparsed` 收非法 token(不静默丢)。**sprite 名契约**(下一步制图必须产出同名文件):`web/assets/icons/{thermal-pool,splitter,merger,belt-straight,belt-curve,bridge}.png`。验证:[web/test/blueprint-smoke.mjs](../../../web/test/blueprint-smoke.mjs)(`node web/test/blueprint-smoke.mjs`)拿两个种子的真实 `操作步骤` 行断言——`stepCount` == 打印的 N、splitter 数 == 步数、merge 汇流器数 == 🟢 数(4号谷地 4 / 武陵 3)、source+entry 在最前、2/3 口正确、`3🔴×2` 展开成 2 步、空串/非法 token 不崩,28/28 全绿;`node --check web/blueprint.js` 过;loader-smoke / page-config-smoke 仍全绿(纯新增文件,未动既有路径)。
2. ⬜ **render-ready sprite → `web/assets/icons/`**。产出步骤 #1 契约里的 6 个 png(tile 对齐、统一尺寸、去游戏 UI chrome)。**开放点**(下一轮先定):`.scratch/icons/` 的工作裁图里 splitter/merger/bridge 含相邻 tile 与网格线(chrome 污染),低保真但能用;干净重裁需 **3638×1826 源截图**(`Screenshot 2026-06-08 ...png`,**当前不在仓库内**,只有用户处有)。「逻辑示意」保真度下,可先用裁图收紧后凑合,或请用户补源截图重裁。
3. ⬜ **DOM 渲染器(blueprint.js,`typeof document` guard)**:吃 #1 的 `nodes`,排成 flex 横链,tile 间插直传送带 sprite,splitter 显 2/3 badge + 红/绿 tint。需真实浏览器目视(`chrome-cdp`,须用户批准),归入下方「待确认」的浏览器交互验证一并做。
4. ⬜ **集成进页面**:`app.js` 提交计算后用 `extractStepsLine(stdout)` → `parseSteps` → 渲染到 [index.html](../../../web/index.html) 新容器,与结果区并列。

### 已定设计点(原「待定」已解,2026-06-08)

- **数据源 = 选项 A**:直接用 `操作步骤` 打印串的分组顺序渲染,零 Swift 改动(用户未要求物理保真,故不走选项 B 的有序 `steps` JSON)。打印串本身把 preSplitBits 的 `阻流` 段排在最前,线性渲染天然满足「阻流器段在前」。
- **布局 = 左→右线性链**:最简起步,蛇形折行留作后续增强。

### 6 个 sprite(从截图裁,源 `Screenshot 2026-06-08 ...png`,3638×1826)— 已全部确认

`1 热能池` · `2 三分分流器` · `3 三合一汇流器` · `4 直传送带` · `5 转弯传送带` · `6 物流桥`(belt 立体交叉,垂直两向都有带通过)。**无独立二分分流器**:`2🔴`/`2🟢` 两分步物理上用三分分流器只接 2 口实现,渲染用同一 sprite + 标注区分 2/3。工作裁图在 `.scratch/icons/`(gitignored,`splitter`/`merger` 预览裁框略偏);render-ready sprite(tile 对齐、统一尺寸、可能去背)实现时产出,脱敏后存 `web/assets/icons/`,不放 `~/`。

### 验证

- 纯核心(子任务 #1):`node web/test/blueprint-smoke.mjs` 对真实 `操作步骤` 行断言渲染规则(已绿)。
- 渲染器 + 集成(子任务 #3/#4):真实浏览器目视——sprite 总数与步骤数一致、每个 🟢 配一个汇流器、source+入口汇流在最前、2/3 口分流器用对 sprite。
