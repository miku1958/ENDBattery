# question — swift-wasm-web

已确认(已并入 `TODO.md`):输入用 stdin JSON;页面放仓库根目录;config 存 localStorage 且网页可切换多个 config。

## 唯一待最终确认:`.wasm` 怎么上线

你想要"每次自动更新"。澄清:网页**不能**直接从 GitHub Actions artifact 下载(URL 不公开、需认证、会过期)。可行的等价做法是 Actions 构建后**部署到 Pages**。两条路二选一:

- 选项 A:本地装 wasm SDK 编译,把 `.wasm` 手动提交进仓库根目录,Pages 从 main 根目录服务。简单,但二进制进 git、每次改 Swift 要本地重编再提交。
- 选项 B(**推荐,匹配"自动更新"**):写一个 GitHub Actions workflow,push Swift 改动后 CI 自动编译 `.wasm` 并部署到 Pages(Pages source 设为 "GitHub Actions")。本地不用装 wasm SDK,`.wasm` 不进 git,网页永远是最新。代价是首次配 workflow + CI 每次构建较慢。

html/js 源文件两种方案都放仓库根目录,不受影响。

回答 A 还是 B 后即可开始实现。
