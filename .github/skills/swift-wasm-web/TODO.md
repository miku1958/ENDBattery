# TODO — swift-wasm-web

## 本次任务

把 ENDBattery 编译成 WebAssembly,加一个**交互式**网页(HTML 表单填参数 → WASM 计算 → 显示结果),托管在 GitHub Pages。

已确认设计:
- 交互式方案(非"只跑固定配置"):网页表单提供电池类型、`baseRequiredPower` 及选填项,计算结果显示在页面上。
- 因此需要重构 `Sources/ENDBattery/main.swift`,把硬编码 `configs` 改为从输入读取,WASM 端解析;保留原 CLI 用法。
- 输入传递:**stdin 喂一段 JSON**,Swift 端解码(对应 question 选项 B)。
- 页面位置:仓库**根目录**(html/js 源文件放根目录)。
- config 存储:浏览器 **localStorage** 保存;网页 UI 支持保存 / 切换多个 config(用户可建多个命名配置并切换)。
- `.wasm` 上线方式(question 问题 2)仍待最终确认,见 `question.md`。

## 待办(按执行顺序)

1. 装 swiftly 6.3.x toolchain + 完整版 wasm Swift SDK,验证 `swift build -c release --swift-sdk <id>` 能编出当前未改动代码的 `.wasm`(优先排查 Foundation 兼容问题)。此步不依赖任何待确认项,可先做。
2. 设计输入 schema 并重构 `main.swift`:硬编码 `configs` → 从输入读取(传参方式见 `question.md`)。保留 CLI 用法,改动前后用 `swift run -c release` 对比输出无回归。
3. 浏览器侧 loader:用 `@bjorn3/browser_wasi_shim` 加载 `.wasm`、传参、捕获 stdout。
4. HTML 表单(电池类型、`baseRequiredPower`、各选填项)+ 结果展示区。
5. 部署:静态资源放仓库目录并开 GitHub Pages(目录 / `.wasm` 提交方式见 `question.md`)。
6. 端到端验证:本地静态服务器跑通,确认网页输出与同参数 CLI 输出一致。

## 阻塞

第 2、5 步开始前需要 `question.md` 里的设计决策;第 1 步可立即开始。
