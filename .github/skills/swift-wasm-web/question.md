# question — swift-wasm-web

实现前需要你拍板的设计分叉(高层"交互式网页"方向已确认):

1. **输入传递方式**:WASM 端怎么收参数?
   - 选项 A:命令行参数(WASI `args`),网页拼成 argv。
   - 选项 B:stdin 喂一段 JSON,Swift 端解码。
   - 建议:B(参数多、结构化,扩展性好)。

2. **`.wasm` 怎么上线**:
   - 选项 A:把构建好的 `.wasm` 直接提交进仓库(简单,但二进制进 git)。
   - 选项 B:用 GitHub Actions 构建后发布到 Pages(仓库干净,但要配 CI)。
   - 建议:A(项目小,先跑通)。

3. **页面放哪 / Pages 来源**:`docs/` 目录(main 分支)还是单独 `gh-pages` 分支?
   - 建议:`docs/`(改动和源码同分支,review 方便)。

4. **网页计算范围**:CLI 现在一次跑多个 config(`4号谷地`、`武陵`…)。网页是单个 config 算一次,还是保留批量?
   - 建议:单 config 算一次(交互式更直观)。

回答后我会把决定整理进 `TODO.md` 并开始实现。
