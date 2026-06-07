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

### 3. 浏览器侧 loader

用 WASI polyfill 加载 `.wasm`,把模块写到 stdout 的字节流捕获后渲染到页面。SwiftWasm 生态标准选型是 `@bjorn3/browser_wasi_shim`:构造带 stdout 捕获的 WASI 实例 → `WebAssembly.instantiate` → 调用 `_start` → 收集 stdout。具体 JS API 在实现阶段对照其 README 核对,不凭记忆写。

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

### 5. 部署(GitHub Actions → Pages)

html/js 静态源文件放仓库**根目录**。`.wasm` **不进 git**:写一个 GitHub Actions workflow,装 swiftly + wasm SDK → `swift build -c release --swift-sdk <id>` → 把 `.wasm` 与静态资源打包 → 部署到 Pages。仓库 Settings 里 Pages source 设为 "GitHub Actions"。push Swift 改动后 CI 自动重编重部,网页永远是最新。

## Validation

- `swift test` 跑通已迁移场景(原硬编码 config 的期望输出)。
- 本地起静态服务器(如 `python3 -m http.server`)打开页面,填一组参数,确认输出与对应测试场景一致。
- push 后确认 Actions 构建成功、Pages 站点可访问。
- 浏览器 DevTools Network 确认 `.wasm` 以 `application/wasm` 返回、无 404。

## Constraints / Safety

- 提交进仓库的样本 / 截图脱敏。
- 不为绕过问题加 COOP/COEP 之类无关配置(本项目不需要)。
- 按本项目 CLAUDE.md 启用的 `local-detailed-logging`:构建 / 验证排障优先看本地日志,日志文件进 `.gitignore`。

## References

- swift.org《Getting Started with Swift SDKs for WebAssembly》: https://www.swift.org/documentation/articles/wasm-getting-started.html
- `@bjorn3/browser_wasi_shim`(浏览器 WASI 运行时,stdout 捕获)。
