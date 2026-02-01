# Issue Analyzer 使用说明

本仓库包含一个 Jira 抓取脚本 jira_scraper.py，用于从 Apache Jira 的 YARN 项目抓取所有 Issue，用于后续分析。依赖与基本运行方式请参阅 requirements.txt 与本 README。

依赖
- 运行时依赖：Python 3、requests。请在 requirements.txt 中查看并使用虚拟环境安装。
- 开发依赖：requirements-dev.txt（如有需要）.

快速开始
1) 创建并激活虚拟环境
```bash
python3 -m venv .venv
source .venv/bin/activate  # macOS/Linux
# Windows:
.
\\.venv\\Scripts\\activate
```

2) 安装依赖
```bash
pip install -U pip
pip install -r requirements.txt
```

3) 运行 Jira 抓取脚本
 - 以默认配置抓取 Yarn 项目的已解决 Issue（resolved），并输出 JSON 文件
 ```bash
 python jira_scraper.py --project YARN
 ```
 - 需要认证时，传入用户名/令牌：
 ```bash
 python jira_scraper.py --project YARN --username YOUR_EMAIL --token YOUR_API_TOKEN
 ```

- 4) 输出说明
- yarn_issues.json：包含标准化后的 Issue 条目列表（键、描述、摘要、状态、负责人、创建/更新时间、类型、标签、优先级、解决状态、修订版本等字段）。
- yarn_issues.csv（若指定）：提供同样信息的 CSV 版本，便于分析工具导入。

5) 高级用法
- 自定义查询：使用 --jql 指定 Jira Query Language 字符串，覆盖默认的项目过滤条件。
- 分页控制：--max-results 控制每次请求的数量，默认 1000；实际使用时可结合环境速率限制调整。
- API 认证：若 Jira 需要鉴权，请通过 --username、--token 提供凭据。
 - 时间范围抓取：使用 --start-date、--end-date、--date-field 选项来限定抓取的时间区间。若提供时间参数，将被追加到 JQL 中，请确保字段名与 Jira 的时间字段匹配。
 - 示例：
  1) python jira_scraper.py --project YARN --start-date 2025-01-01 --end-date 2025-01-31
  2) python jira_scraper.py --project YARN --start-date 2025-01-01 --end-date 2025-01-31 --date-field updated
  3) python jira_scraper.py --jql "project = YARN AND issuetype = Bug" --start-date 2024-12-01 --end-date 2024-12-31

6) 维护与扩展
- 若新增依赖，请同步更新 requirements.txt，必要时更新 poetry.lock 或 Pipfile.lock。
- 如需要扩展输出字段，请修改 jira_scraper.py 的 fields 参数与 normalize_issue 的解析逻辑。

7) 问题与支持
- 如遇网络问题、认证失败或 API 限流，请检查网络连接、凭据有效性以及 Jira 实例的限制。
- 如需帮助，请参阅 AGENTS.md 的相关通用规范，或联系项目维护者。

8) API 服务
- 提供 REST API 来查询详细的 Issue 信息
- 运行：python api.py
- 端点：
  - GET /issues/<issueid> 返回单条 Issue 的详细信息
  - GET /issues?limit=N 返回最近的 N 条 Issue 的摘要信息
- 数据源：从 result.db 的新表读取字段 issueid、summary、description、status、assignee、created、updated、issuetype、labels、priority、resolution、fixVersions、created_at
- 使用示例：
  - curl http://localhost:8000/issues/YARN-123
  - curl http://localhost:8000/issues?limit=5

9) Reflex 前端集成
- 已添加一个前端骨架用于从后端 API 查询详细的 Issue 信息并显示在页面上，位于 frontend/reflex/Main.hs。
- 该文件当前为起步骨架，演示如何通过 Reflex 与后端 API（/issues 和 /issues/{issueid}）对接。要在浏览器中显示，需要使用 Reflex-DOM 构建并编译为前端 JavaScript。
- 运行前提：需要在本地安装 Reflex-Platform / GHCJS 相关工具链，具体构建步骤请参考 Reflex 官方文档。
- 未来工作：将此骨架扩展为完整的交互式 UI（输入 IssueID、列出字段、实时刷新等），并将前端静态资源集成到项目中。

版本
- 当前版本：1.0.0
