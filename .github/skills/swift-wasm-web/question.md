# 待用户确认 — swift-wasm-web

三个待确认问题已于 2026-06-09 由用户拍板,决定已并入 [TODO.md](TODO.md):

1. **浏览器交互验证** → 由用户负责 review HTML 并给出修改内容,不用 `chrome-cdp`(见 TODO 第四轮 #3 与「待确认」段)。
2. **GitHub Actions Node 24 升级** → 现在就升,bump 四个 JS action 到各自当前最新 major(见 TODO「GitHub Actions 升级到 Node 24」段)。
3. **蓝图图标** → 原"从游戏截图裁 PNG sprite"方案作废,改为手绘示意 **SVG** 存 `web/assets/icons/*.svg`(见 TODO 第四轮 #2)。

剩一件仍需用户决定:

## README 陈旧:重写、删除,还是暂不动?

[README.md](../../../README.md) 整篇仍在描述 round 1 已删除的产品级 CLI(`swift run` 跑硬编码
`Config.myBase`、`swiftc Sources/ENDBattery/main.swift`、旧输出 `所需电池数量:`、旧电池名表),
与现在的 HTML/WASM 网页产品完全不符。这是 round 1 删 CLI 时漏改的整篇陈旧文档,不在任何一轮
改动范围内,需用户拍板怎么处理:

- 选项 A(推荐):按现产品重写 README(网页 + WASM 用法、本地预览、部署说明)。
- 选项 B:直接删掉 README。
- 选项 C:暂不动,继续记在案。

**不阻塞第四轮**:据上面已拍板的设计,我可继续画 SVG 图标、写 DOM 渲染器、集成进页面;
README 决定与第四轮并行,任你随时定。
