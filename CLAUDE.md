# ENDBattery

终末地电力优化计算器:Swift Package Manager 命令行工具,纯计算 + `print` 输出,入口 [Sources/ENDBattery/main.swift](Sources/ENDBattery/main.swift)。

## 额外遵循的外部规则

本项目遵循以下规则文件,等同于把其正文内容并入本项目 CLAUDE.md:

@~/.aiGlobal/rules-optional/commit-after-task.md
@~/.aiGlobal/rules-optional/local-detailed-logging.md
@~/.aiGlobal/rules-optional-code/swift-explicit-initialization.md

## 构建与验证

- CLI 构建运行:`swift run -c release`(改动前后各跑一次,对比输出确认无回归)。
- 更细的 Swift 编码约定见 [.github/copilot-instructions.md](.github/copilot-instructions.md)。

## 进行中的工作

- Swift → WebAssembly + 交互式网页(GitHub Pages):见 [.github/skills/swift-wasm-web/SKILL.md](.github/skills/swift-wasm-web/SKILL.md) 与同目录 `TODO.md`、`question.md`。
