# AGENTS 开发规范指南

Issue Analyzer - Jira Issue 抓取系统

## 项目结构

| 文件 | 用途 |
|------|------|
| `jira_scraper.py` | Jira 数据抓取（含重试机制和 429 限流处理） |
| `db_writer.py` | SQLite 数据库操作（WAL 模式，threading.Lock 并发保护） |
| `api.py` | Flask REST API + Bootstrap 前端 |
| `config.py` | 配置管理（环境变量优先） |
| `cache.py` | 简单内存缓存（TTL 60s） |
| `templates/` | Jinja2 模板（Bootstrap 5） |

- 数据库位置：`data/result.db`

## 启动命令

```bash
# 抓取 Jira（默认项目：HDFS,YARN,HADOOP,MAPREDUCE,ZOOKEEPER）
python jira_scraper.py --project YARN

# 运行 API 服务
python api.py

# 访问前端
http://localhost:8000/
```

## API 端点

- `GET /` - 首页（支持 project、field、value 筛选）
- `GET /marked` - 已标记的 issues
- `GET /issue/<issueid>` - 问题详情页
- `POST /issue/<issueid>/toggle` - 切换标记状态
- `GET /issues?limit=N&offset=M&field=X&value=Y` - REST API 列表
- `GET /issues/<issueid>` - REST API 详情
- `POST /issues/<issueid>/markdetail` - 更新 markdetail 字段

## 代码质量

```bash
# 静态检查（推荐）
ruff check .

# 格式化 + 排序 imports
ruff check . --fix
```

## 配置说明

所有配置通过环境变量或 config.py 设置：
- `MAX_RETRIES` - 重试次数（默认 10）
- `RETRY_DELAY` - 重试延迟秒数（默认 3）
- `REQUEST_INTERVAL` - 请求间隔（默认 1.0）
- `DEFAULT_LIMIT` - 默认分页大小（默认 20）
- `MAX_LIMIT` - 最大分页上限（默认 1000）
- `CACHE_TTL` - 缓存有效期（默认 60s）

## Jira Scraper 特定规则

- 跳过的 resolution：Won't Fix, Duplicate, Not A Problem, Not A Bug, Won't Do, Invalid, Delivered, Abandoned, Information Provided, Incomplete
- JQL 日期处理：使用 `_date_literal()` 包裹日期字符串
- 429 限流：自动读取 Retry-After header

## 数据库字段

issues 表：issueid, project_name, summary, description, status, assignee_name, assignee_email, created, updated, issuetype, labels, priority, resolution, fixVersions, markdetail

## 数据库索引

- idx_project_name (project_name)
- idx_status (status)
- idx_created (created)

## GitHub CI/CD

- 工作流：`.github/workflows/ci.yml`
- Jobs：lint, test
- 触发：push/PR 到 main 或 develop

## 开发约定

- 路由函数保持简洁，复杂逻辑抽离到 db_writer
- SQL 查询使用参数化，禁止字符串拼接
- 变量命名使用 snake_case（如 `fix_versions`，不是 `fixVersions`）
- 新功能开发使用 Subagent-Driven 方式

## 实施计划执行规则

- 默认使用 **Subagent-Driven** 方式执行计划（每个任务派遣独立子任务，分阶段审查）
- 只有简单任务（单文件修改、10分钟以内可完成）可使用 Inline Execution

## 分支保护规则

**禁止直接 push 到 main/master 分支**
- 所有更改必须通过 Pull Request 合并
- 创建功能分支：`git checkout -b feature/xxx` 或 `git checkout -b fix/xxx`
- 提交后推送分支：`git push -u origin branch-name`
- 通过 PR 合并到 main
