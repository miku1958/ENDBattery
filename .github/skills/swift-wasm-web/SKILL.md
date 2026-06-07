---
name: swift-wasm-web
description: 把 ENDBattery (Swift SPM CLI) 交叉编译成 WebAssembly/WASI 并做成交互式网页跑在浏览器 / GitHub Pages。涉及 swiftly、wasm Swift SDK、swift build --swift-sdk、WASI、browser_wasi_shim、捕获 stdout、GitHub Pages 部署、为网页输入重构 main.swift 时使用。
---

# Swift → WebAssembly 交互式网页

把本仓库的 Swift 命令行计算器编译成 WASM,在浏览器里用表单填参数、跑计算、显示结果,托管在 GitHub Pages。

## When to Use

- 需要把本项目(或其它纯计算的 Swift SPM 可执行)交叉编译成 `.wasm`。
- 需要在浏览器 / GitHub Pages 里运行 Swift 编出的 WASI 模块并捕获其 stdout。
- 需要为网页输入改造 `main.swift` 的硬编码 `configs`。

不适用:涉及线程 / 文件系统 / 网络 / `readLine` 的 Swift 代码(WASI 下能力受限,需先评估);非计算型、依赖系统 framework 的代码。

## 架构决策

- 产品入口只有 HTML/WASM,不保留产品级 CLI。计算逻辑抽成可被测试 import 的 library target;`ENDBattery` executable 只做 WASM 入口(stdin JSON → 计算 → print)。
- 本地验证与 debug 走 `swift test`;原硬编码 `4号谷地`/`武陵` 场景迁为测试用例。

### 已落地的 target 布局(Package.swift 三 target)

- `Sources/ENDBatteryCore/`(library):`Calculator.swift` 装搜索 / 模拟逻辑 + `public func runCalculation(input:)`;`Input.swift` 装 `public struct CalculatorInput: Decodable` + `Config`/`Config.Battery` 的 Decodable + `applyOptions(_:)`。
- `Sources/ENDBattery/main.swift`(executable,依赖 ENDBatteryCore):读 stdin → `JSONDecoder().decode(CalculatorInput.self,...)` → `runCalculation(input:)` → print;计时留在这里(库不碰 `Date()`,保证输出确定性)。
- `Tests/ENDBatteryCoreTests/`:`4号谷地`/`武陵` + 默认值场景,用 dup2 捕获 stdout 后断言关键行(对照 `logs/baseline-current-output.txt` 基线)。
- 原硬编码 `configs` 全局 + 各 `选填` 全局保留为 `ENDBatteryCore` 模块级 `var`,`applyOptions` 每次运行从输入覆盖。之所以走模块级 `var` 而非把参数穿进每个函数:`maxShortageDurationLimitInSecond`/`safetyThreshold`/`coreMaxCapacity` 被 `OverlapProfile.<`(`Comparable` 静态方法,签名固定)引用,无法以参数注入,模块级状态是对原 file-level 全局最忠实的翻译。

## 关键事实(已核对 swift.org 官方 WASM 文档)

- 系统 Xcode 自带的 Swift **不含** wasm Swift SDK;必须用 swiftly 装独立 toolchain,且 SDK 版本要和 toolchain **精确匹配**。
- 本项目依赖 Foundation(`Date`、`String(format:)`)→ 必须用**完整版** wasm SDK(ID 形如 `swift-<ver>-RELEASE_wasm`),**不要**用 `-embedded` 那个(子集,不含完整 Foundation)。已实测:完整版 SDK 下 `Date`/`String(format:)` 的**编译与 WasmKit 运行均正常**,当前未改动代码可直接交叉编译并跑出正确结果。
- 计算核心无线程 / 文件 / 网络 / `readLine` / 并发,对 WASI 友好。
- **已实测**:`FileHandle.standardInput.readDataToEndOfFile()` + `JSONDecoder` 在 WASI/WasmKit 下可用 —— `echo '<json>' | swift run -c release --swift-sdk <id> ENDBattery` 端到端跑出与 host 字节一致的结果。即 stdin JSON 输入路径在 WASM 侧成立,浏览器侧只需 WASI 运行时把表单 JSON 喂进 stdin。
- 本项目不用 SharedArrayBuffer / 线程 → 浏览器侧**不需要** COOP/COEP 响应头,GitHub Pages 默认即可运行。

## Procedure

### 1. 装 swiftly 本体 + toolchain + wasm SDK

```bash
# a. 装 swiftly 本体(若 `swiftly` 不存在):macOS 官方 pkg 装进用户目录,不需 sudo
curl -fSL -o swiftly.pkg https://download.swift.org/swiftly/darwin/swiftly.pkg
installer -pkg swiftly.pkg -target CurrentUserHomeDirectory
~/.swiftly/bin/swiftly init --no-modify-profile --skip-install --assume-yes --quiet-shell-followup
# --no-modify-profile 不动用户 shell profile;之后每个 shell 先激活再用 swift:
. ~/.swiftly/env.sh && hash -r

# b. 装 toolchain + wasm SDK
swiftly install 6.3.2 --assume-yes   # 装完自动设为 default,无需再 swiftly use
swift sdk install https://download.swift.org/swift-6.3.2-release/wasm-sdk/swift-6.3.2-RELEASE/swift-6.3.2-RELEASE_wasm.artifactbundle.tar.gz \
  --checksum a61f0584c93283589f8b2f42db05c1f9a182b506c2957271402992655591dd7c
swift sdk list   # 完整版 ID: swift-6.3.2-RELEASE_wasm(另列出 -embedded 子集版,本项目不用)
```

> 版本号会随官方更新变化;实际安装前先到官方文档确认当前推荐版本与对应 checksum,见 References。

### 2. 交叉编译

```bash
swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm
# 本地用 WasmKit 试跑:
swift run -c release --swift-sdk swift-6.3.2-RELEASE_wasm
```

产出位于 `.build/wasm32-unknown-wasip1/release/ENDBattery.wasm`(交叉编译产物在 target-triple 子目录下,不是 `.build/release/`)。

### 3. 浏览器侧 loader(已完成,已实测)

`@bjorn3/browser_wasi_shim` v0.4.2 **vendor 进仓库** `vendor/browser_wasi_shim/`(整个 `dist/*.js` + `LICENSE-MIT`),不走 CDN:静态站自包含、无 JS 构建步、浏览器与 Node smoke test 用同一份。更新办法见该目录 `README.md`。

`loader.js`(仓库根)导出 `runWasm(wasmModule, stdinJson) -> {stdout, stderr, exitCode}`,已核对 shim 源码的实测要点:

- fd 数组:`[ new OpenFile(new File(stdinBytes, {readonly:true})), new ConsoleStdout(b=>stdoutChunks.push(b)), new ConsoleStdout(b=>stderrChunks.push(b)) ]` —— fd0 stdin、fd1/fd2 各收**原始字节**(`ConsoleStdout` 回调拿到的是已脱离 wasm 内存的 slice 拷贝),全部跑完再 `TextDecoder` 一次解码,避免 `lineBuffered` 拆多字节 UTF-8。
- **坑(必踩)**:`new WASI(args, env, fds, { debug: false })` 第四参不能省。shim 的 `WASI` 构造里 `debug.enable(options.debug)`,而 `enable(undefined)` 被当成"开",会每次 WASI 调用都 `console.log` 刷屏;必须显式 `{debug:false}`。
- 传**已编译的 `WebAssembly.Module`**(非字节)给 `WebAssembly.instantiate`,它直接 resolve 出 `Instance`;再 `wasi.start(instance)`(内部 `_start()`,捕获 `WASIProcExit` 返回退出码,Swift 正常退出码 0)。
- 浏览器侧编译:`await WebAssembly.compileStreaming(fetch('ENDBattery.wasm'))`;Node 侧:`WebAssembly.compile(await readFile(path))`。

验证用 `test/loader-smoke.mjs`(`node test/loader-smoke.mjs`):无浏览器时用同一 loader + vendored shim 在 Node 跑 wasm,合并两 config 输出去掉计时行后与 `logs/baseline-current-output.txt` 字节一致、单 config 命中 `swift test` 断言行。需先本地交叉编译出 `.wasm`(产物不进 git)。

### 4. 输入重构(已完成)

库 / executable / test 三 target 已拆好(见上「已落地的 target 布局」)。stdin JSON schema:

```json
{
  "configs": [
    { "name": "武陵",
      "staticBattery": [{ "type": "midEarth", "count": 1 }],
      "analyzedBattery": { "type": "purple" },
      "baseRequiredPower": 6210 }
  ],
  "minAnalyzedBatteryCount": 1, "extraBeltInSteps": 1, "maxDepthLimit": 9,
  "showTopSolutions": 1, "allowedMinDiff": 10, "enableThree": true,
  "safetyThreshold": 0.15, "maxStopToOutageSeconds": null,
  "maxShortageDurationLimitInSecond": 1000, "keepAllSolutions": false
}
```

- 电池统一表示 `{ "type": <originium|green|blue|purple|lowEarth|midEarth>, "count": <Int?> }`;`staticBattery` 省略默认 `[]`。
- `configs` 是数组(网页一次算一条就传长度 1;test 借此一条一条迁场景)。
- 除 `configs` 外所有字段在 JSON 里都可省略,缺省回落 `CalculatorInput.init(from:)` 里写死的默认值;网页表单的「选填项」直接 1:1 映射这些字段。
- config 在浏览器用 localStorage 保存,UI 可建 / 切换多个命名 config(待办 #3)。

### 4.5 交互式页面(已完成)

`index.html` + `app.js`(仓库根,与 `loader.js` 同级):

- `app.js` 把纯数据函数(`buildStdinJson(config)`、`SEED_CONFIGS`、`OPTION_SPEC`/`OPTION_DEFAULTS`、`BATTERY_TYPES`)导出,DOM 接线整段用 `if (typeof document !== 'undefined')` guard 包住——这样 Node 测试可只 import 纯函数、不触发 `document`(同 `loader.js` 浏览器/Node 共用一份的思路)。
- 表单:配置名(= `config.name`)、分流电池类型、`baseRequiredPower`、可增删的固定电池行(type + 选填 count);高级选项从 `OPTION_SPEC` 单一来源动态渲染,默认值逐项对齐 `CalculatorInput.init(from:)`,故表单不动时与"省略所有 tunable key"输出一致。`buildStdinJson` 的 `configs` 恒长度 1(一次算一条),固定电池 count 留空则不写 `count` 键。
- 多 config:localStorage key `endbattery.configs.v1` 存一个 config 对象数组;保存按名覆盖、可切换/删除/新建;localStorage 为空时种入 `4号谷地`/`武陵`。
- wasm:`WebAssembly.compileStreaming(fetch('ENDBattery.wasm'))`,catch 后回落 `WebAssembly.compile(arrayBuffer)`(兼容不发 `application/wasm` 的本地服务器);惰性编一次、跨次 `runWasm` 复用同一 Module(每次 `runWasm` 内部新建 instance,模块级状态不串)。

### 5. 部署(GitHub Actions → Pages)

workflow:`.github/workflows/deploy-pages.yml`,两个 job:

- `build`(`container: swift:6.3.2`):用**官方 Swift 发布镜像**而非在 CI 里现装 swiftly —— toolchain 预装,镜像 tag 必须与下面装的 wasm SDK **版本精确一致**(`swift:6.3.2` ↔ `swift-6.3.2-RELEASE_wasm`)。步骤:`swift sdk install <与本地同一 URL+checksum>` → `swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm --product ENDBattery` → 把 `index.html`/`app.js`/`loader.js`/`vendor/` 和产物 `.build/wasm32-unknown-wasip1/release/ENDBattery.wasm` 拷进 `_site/` → `actions/upload-pages-artifact@v3`。
- `deploy`(`runs-on: ubuntu-latest`):`actions/configure-pages@v5`(会尝试自动 enable Pages,source=workflow)+ `actions/deploy-pages@v4`;顶层 `permissions: pages: write` + `id-token: write`。

要点 / 坑:

- **容器 job 不需要镜像自带 node**:GitHub 把 runner 的 node 注入容器(`/__e/node20`),`checkout`/`upload-pages-artifact` 等 JS action 照常跑;swift 镜像(Debian/Ubuntu)自带 `tar`/`gzip`/`cp`,assemble 与打包够用。
- wasm 产物 triple 子目录 `wasm32-unknown-wasip1` 由 SDK 决定、与 host 架构无关,Linux amd64 runner 上路径同 mac。
- **尺寸**:wasm 约 58MB(完整 Foundation,未瘦身)< Pages 单文件 100MB 限;Pages/Fastly 对 `application/wasm` 自动 gzip/br,裸传只剩零头,首版无需瘦身。要再压缩可在 build job 加 binaryen 跑 `wasm-opt -Os`,属增量,非必需。
- `.wasm` **不进 git**(`/ENDBattery.wasm` 在 `.gitignore`),CI 每次 push 重编重放,网页永远最新。
- 仓库 Settings → Pages 的 source 设为 "GitHub Actions" 是一次性前置(`configure-pages` 通常能自动 enable;否则手动设,或 `gh api -X POST repos/<owner>/<repo>/pages -f build_type=workflow`)。

## Validation

- `swift test` 跑通已迁移场景(原硬编码 config 的期望输出)。
- `node test/loader-smoke.mjs`:同一 loader + vendored shim 在 Node 跑 wasm,对照基线。
- `node test/page-config-smoke.mjs`:import `app.js` 的 `buildStdinJson` + `SEED_CONFIGS`,把两个种子配置经 form→JSON→`runWasm` 跑出与基线一致的关键行——验证页面的"选中配置→开始计算"数据路径,无需浏览器。
- 本地起静态服务器(如 `python3 -m http.server`,需先把 `.wasm` 复制到根)打开页面,填一组参数,确认输出与对应测试场景一致。
- push 后确认 Actions 构建成功、Pages 站点可访问。
- 浏览器 DevTools Network 确认 `.wasm` 以 `application/wasm` 返回、无 404。

## Constraints / Safety

- 提交进仓库的样本 / 截图脱敏。
- 不为绕过问题加 COOP/COEP 之类无关配置(本项目不需要)。
- 按本项目 CLAUDE.md 启用的 `local-detailed-logging`:构建 / 验证排障优先看本地日志,日志文件进 `.gitignore`。

## References

- swift.org《Getting Started with Swift SDKs for WebAssembly》: https://www.swift.org/documentation/articles/wasm-getting-started.html
- `@bjorn3/browser_wasi_shim`(浏览器 WASI 运行时,stdout 捕获)。
