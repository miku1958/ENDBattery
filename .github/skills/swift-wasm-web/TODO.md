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
3. **← 当前**。HTML 表单(电池类型、`baseRequiredPower`、各选填项)+ 结果展示区 + localStorage 多 config 保存 / 切换 UI。表单收集字段拼成 loader 的 stdin JSON(schema 见 SKILL.md 第 4 步),调 `runWasm` 后把 `stdout` 渲染到结果区。浏览器侧需 `WebAssembly.compileStreaming(fetch('ENDBattery.wasm'))` 编出 Module 再传给 `runWasm`。
4. 部署:html/js 放仓库根目录;写 GitHub Actions workflow(装 swiftly + wasm SDK → build → 打包 `.wasm` + 静态资源 → 部署 Pages);仓库设置里把 Pages source 改为 "GitHub Actions"。**实测产物 `ENDBattery.wasm` 约 60MB**(完整版 Foundation SDK,未瘦身),裸下载偏大;部署阶段需评估 `-Osize` / `wasm-opt` 瘦身与 gzip/br 压缩(Pages 默认对 `application/wasm` 会压缩传输),必要时按「需求含糊先问清」与用户确认取舍。
5. 端到端验证:`swift test` 跑通已迁移场景;本地静态服务器跑通网页,确认输出与对应测试场景一致;推送后确认 Actions 部署成功、Pages 站点可访问。
