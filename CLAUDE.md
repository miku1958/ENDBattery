# ENDBattery

终末地电力优化计算器:核心计算用 Swift 写成,交叉编译为 WebAssembly,产品是交互式网页(托管在 GitHub Pages)。整个 SPM 包在 [swift/](swift/),前端在 [web/](web/);WASM 入口 [swift/Sources/ENDBattery/main.swift](swift/Sources/ENDBattery/main.swift) 读 stdin JSON → 计算 → `print`。

## 额外遵循的外部规则

本项目遵循以下规则文件,等同于把其正文内容并入本项目 CLAUDE.md:

@~/.aiGlobal/rules-optional/commit-after-task.md
@~/.aiGlobal/rules-optional/local-detailed-logging.md
@~/.aiGlobal/rules-optional-code/swift-explicit-initialization.md

## 构建与验证

- 产品入口是 HTML/WASM;本地验证与 debug 走 `swift test`(在 `swift/` 目录下跑)。
- 更细的 Swift 编码约定见 [.github/copilot-instructions.md](.github/copilot-instructions.md)。

## 进行中的工作

- Swift → WebAssembly + 交互式网页(GitHub Pages):见 [.github/skills/swift-wasm-web/SKILL.md](.github/skills/swift-wasm-web/SKILL.md) 与同目录 `TODO.md`。
