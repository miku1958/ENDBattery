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

## 待办(按执行顺序)

1. 装 swiftly 6.3.x toolchain + 完整版 wasm Swift SDK,验证 `swift build -c release --swift-sdk <id>` 能编出当前未改动代码的 `.wasm`(优先排查 Foundation 兼容问题)。
2. 拆分 Package:计算逻辑 → library target;`ENDBattery` executable 改为从 **stdin JSON** 读取 → 调用 library → print;新增 test target,把硬编码 `4号谷地`/`武陵` 场景迁为测试用例。移除产品级 CLI 用途。
3. 浏览器侧 loader:用 `@bjorn3/browser_wasi_shim` 加载 `.wasm`、喂 stdin JSON、捕获 stdout。
4. HTML 表单(电池类型、`baseRequiredPower`、各选填项)+ 结果展示区 + localStorage 多 config 保存 / 切换 UI。
5. 部署:html/js 放仓库根目录;写 GitHub Actions workflow(装 swiftly + wasm SDK → build → 打包 `.wasm` + 静态资源 → 部署 Pages);仓库设置里把 Pages source 改为 "GitHub Actions"。
6. 端到端验证:`swift test` 跑通已迁移场景;本地静态服务器跑通网页,确认输出与对应测试场景一致;推送后确认 Actions 部署成功、Pages 站点可访问。
