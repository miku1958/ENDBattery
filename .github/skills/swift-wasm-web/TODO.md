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
swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm -j 8   # 产物: .build/wasm32-unknown-wasip1/release/ENDBattery.wasm
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

- **真实浏览器交互验证**:表单渲染 / localStorage 持久化 / 多 config 切换-保存-删除 这些**纯 DOM 交互**尚未在真实 Chrome 里实测过(纯数据/计算路径已被 `page-config-smoke` + 线上 wasm 实测覆盖)。要实测需用 `chrome-cdp` 驱动真实浏览器,而该能力须用户**显式批准**后才可用。问题与 Node 20 弃用提醒一并记入 `question.md`。
