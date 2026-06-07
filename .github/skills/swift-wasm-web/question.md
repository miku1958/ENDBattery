# 待用户确认 — swift-wasm-web

任务主体已交付并上线验证(见 [TODO.md](TODO.md) #5、[logs/08-pages-deploy-verify.log](../../../logs/08-pages-deploy-verify.log))。
站点:https://miku1958.github.io/ENDBattery/ 。剩两件需要用户拍板:

## 1. 是否要在真实浏览器里实测交互 UI?

纯计算/数据路径(form → JSON → wasm → 输出)已被 `node test/page-config-smoke.mjs`
和"下载线上 wasm 跑种子配置"两次实测覆盖。**未**实测的是纯 DOM 交互:表单渲染、
localStorage 持久化、多 config 切换/保存/删除。要在真实 Chrome 里跑这步得用
`chrome-cdp` skill,而它**须用户显式批准**才可启用。

- 选项 A:批准用 `chrome-cdp` 驱动真实浏览器跑一遍交互冒烟(我会截图佐证)。
- 选项 B(默认):认为线上 + 单测覆盖已足够,不做浏览器实测,任务收尾。

## 2. 是否现在升级 GitHub Actions 版本以消除 Node 20 弃用警告?

最新绿色 run 仍带警告:`checkout@v4`/`upload-artifact@v4`/`configure-pages@v5`/
`deploy-pages@v4` 跑在 Node 20,GitHub 将于 **2026-06-16** 强制迁到 Node 24。
当前部署**正常**,警告非致命。是否要现在主动升级 action 版本(独立小改动),还是
等真出问题再升?默认:暂不动,记录在案。
