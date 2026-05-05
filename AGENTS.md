# AGENTS 开发规范指南

Issue Analyzer - Jira Issue 抓取系统

## 项目结构

| 文件 | 用途 |
|------|------|
| `jira_scraper.py` | Jira 数据抓取 |
| `db_writer.py` | SQLite 数据库操作（threading.Lock 并发保护） |
| `api.py` | Flask REST API + Bootstrap 前端 |
| `templates/` | Jinja2 模板（Bootstrap 5） |

- 数据库位置：`data/result.db`

## 启动命令

```bash
# 抓取 Jira（默认项目：HDFS,YARN,HADOOP,MAPREDUCE,ZOOKEEPER）
python jira_scraper.py --project YARN

# 运行 API 服务（含 Bootstrap UI）
python api.py

# 访问前端
http://localhost:8000/
```

## API 端点

- `GET /issues?limit=N&offset=M&field=X&value=Y` - 列表查询（支持分页和筛选）
- `GET /issues/<issueid>` - 单条详情
- `POST /issues/<issueid>/markdetail` - 更新 markdetail 字段

## 代码质量

```bash
black . && isort .    # 格式化
ruff check .         # 静态检查
mypy .               # 类型检查
```

## Jira Scraper 特定规则

- 跳过的 resolution：Won't Fix, Duplicate, Not A Problem, Not A Bug, Won't Do, Invalid, Delivered, Abandoned, Information Provided, Incomplete
- JQL 日期处理：使用 `_date_literal()` 包裹日期字符串

## 数据库字段

issues 表：issueid, project_name, summary, description, status, assignee_name, assignee_email, created, updated, issuetype, labels, priority, resolution, fixVersions, markdetail

## 开发约定

- 路由函数保持简洁，复杂逻辑抽离到 db_writer
- SQL 查询使用参数化，禁止字符串拼接
- 环境变量写入 .env 文件

## 实施计划执行规则

- 默认使用 **Subagent-Driven** 方式执行计划（每个任务派遣独立子任务，分阶段审查）
- 只有简单任务（单文件修改、10分钟以内可完成）可使用 Inline Execution
