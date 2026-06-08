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

## 3.(第四轮 子任务 #2 阻塞)render-ready sprite 需要干净源截图

第四轮可视化的下一步是产出 6 个 sprite(`web/assets/icons/{thermal-pool,splitter,
merger,belt-straight,belt-curve,bridge}.png`,名字契约见 [TODO.md](TODO.md) 第四轮 #1)。
**卡住**:`.scratch/icons/` 里的工作裁图无法直接用——我逐张看过,污染严重:

- `1-thermal-pool`:中心电池+闪电 glyph 周围全是 UI 面板(右侧两个建造列表框)、网格线、
  上下梯形轮廓和左侧黑色面板边框。
- `2-splitter-3`:分流器主体被左边缘**切掉**,右半是相邻 tile 的斜纹网格 + 竖条。
- `merger/belt/bridge` 同样混入相邻 tile 与网格线。

这些不是 tile 对齐的干净 glyph,而是含 UI chrome、被切断的截图碎片。**视觉/像素规则**禁止
靠感觉猜 tile 边界去硬抠,所以无法从现有裁图可靠产出干净 sprite。要做对这一步,需要
**干净源截图**(`Screenshot 2026-06-08 ...png`,3638×1826),它**当前不在仓库内,只在你那边**。

请选一条路(我据此继续第四轮 #2):

- 选项 A(推荐):把那张 3638×1826 源截图放进 workspace(例如 `.scratch/`),我据真实像素
  精确重裁出 6 张 tile 对齐、去 chrome 的 sprite,脱敏后存 `web/assets/icons/`。
- 选项 B:改用纯 CSS/SVG 画 6 个示意图标(不再"从游戏截图提取 sprite")。这会**改动你已
  确认的设计**(原话要求用游戏截图 sprite),"逻辑示意"保真度下技术上可行,但需你点头改设计。
- 选项 C:坚持用现有污染裁图凑合。**不建议**——含 UI 面板/网格线/被切断的形状,即便"逻辑
  示意"保真度也会显得破碎。

在你回答前,第四轮 #2/#3/#4 都无法推进(#3 渲染器还另需 `chrome-cdp` 目视,见本文件 #1)。
