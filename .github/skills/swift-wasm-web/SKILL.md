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

## 关键事实(已核对 swift.org 官方 WASM 文档)

- 系统 Xcode 自带的 Swift **不含** wasm Swift SDK;必须用 swiftly 装独立 toolchain,且 SDK 版本要和 toolchain **精确匹配**。
- 本项目依赖 Foundation(`Date`、`String(format:)`)→ 必须用**完整版** wasm SDK(ID 形如 `swift-<ver>-RELEASE_wasm`),**不要**用 `-embedded` 那个(子集,不含完整 Foundation)。
- 当前 `Sources/ENDBattery/main.swift` 无线程 / 文件 / 网络 / `readLine` / 并发,对 WASI 友好;但 `configs` 是硬编码的,要做成交互式网页必须改为从输入读取。
- 本项目不用 SharedArrayBuffer / 线程 → 浏览器侧**不需要** COOP/COEP 响应头,GitHub Pages 默认即可运行。

## Procedure

### 1. 装 toolchain + wasm SDK

```bash
swiftly install 6.3.2 && swiftly use 6.3.2
swift sdk install https://download.swift.org/swift-6.3.2-release/wasm-sdk/swift-6.3.2-RELEASE/swift-6.3.2-RELEASE_wasm.artifactbundle.tar.gz \
  --checksum a61f0584c93283589f8b2f42db05c1f9a182b506c2957271402992655591dd7c
swift sdk list   # 记下完整版 ID: swift-6.3.2-RELEASE_wasm
```

> 版本号会随官方更新变化;实际安装前先到官方文档确认当前推荐版本与对应 checksum,见 References。

### 2. 交叉编译

```bash
swift build -c release --swift-sdk swift-6.3.2-RELEASE_wasm
# 本地用 WasmKit 试跑:
swift run -c release --swift-sdk swift-6.3.2-RELEASE_wasm
```

产出位于 `.build/release/ENDBattery.wasm`。

### 3. 浏览器侧 loader

用 WASI polyfill 加载 `.wasm`,把模块写到 stdout 的字节流捕获后渲染到页面。SwiftWasm 生态标准选型是 `@bjorn3/browser_wasi_shim`:构造带 stdout 捕获的 WASI 实例 → `WebAssembly.instantiate` → 调用 `_start` → 收集 stdout。具体 JS API 在实现阶段对照其 README 核对,不凭记忆写。

### 4. 输入重构

把硬编码 `configs` 改为从 **stdin JSON** 读取,Swift 端解码,网页表单据此拼 JSON。保留原 CLI 用法不破坏。config 在浏览器用 localStorage 保存,UI 可建 / 切换多个命名 config。

### 5. 部署(GitHub Actions → Pages)

html/js 静态源文件放仓库**根目录**。`.wasm` **不进 git**:写一个 GitHub Actions workflow,装 swiftly + wasm SDK → `swift build -c release --swift-sdk <id>` → 把 `.wasm` 与静态资源打包 → 部署到 Pages。仓库 Settings 里 Pages source 设为 "GitHub Actions"。push Swift 改动后 CI 自动重编重部,网页永远是最新。

## Validation

- 本地起静态服务器(如 `python3 -m http.server`)打开页面,填一组参数,确认输出与 `swift run -c release` 跑同参数的 CLI 输出一致。
- push 后确认 Actions 构建成功、Pages 站点可访问。
- 浏览器 DevTools Network 确认 `.wasm` 以 `application/wasm` 返回、无 404。

## Constraints / Safety

- 提交进仓库的样本 / 截图脱敏。
- 不为绕过问题加 COOP/COEP 之类无关配置(本项目不需要)。
- 按本项目 CLAUDE.md 启用的 `local-detailed-logging`:构建 / 验证排障优先看本地日志,日志文件进 `.gitignore`。

## References

- swift.org《Getting Started with Swift SDKs for WebAssembly》: https://www.swift.org/documentation/articles/wasm-getting-started.html
- `@bjorn3/browser_wasi_shim`(浏览器 WASI 运行时,stdout 捕获)。
