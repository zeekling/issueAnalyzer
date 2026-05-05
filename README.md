# Issue Analyzer 使用说明

本仓库提供从 Apache Jira 抓取、存储、查询和浏览 Jira issues 的工具链。

## 核心组件

- `jira_scraper.py` - Jira 数据抓取（含重试机制）
- `db_writer.py` - SQLite 存储与查询
- `api.py` - Flask REST API + Bootstrap 前端
- `templates/` - Jinja2 模板（Bootstrap 5）

## 快速开始

### 环境与安装

```bash
python3 -m venv .venv
# Windows: .\.venv\Scripts\activate
# macOS/Linux: source .venv/bin/activate

pip install -U pip
pip install -r requirements.txt
```

### 运行

```bash
# 抓取 Jira（默认项目：HDFS,YARN,HADOOP,MAPREDUCE,ZOOKEEPER）
python jira_scraper.py --project YARN

# 运行 API 服务
python api.py
```

### 访问

打开浏览器访问 http://localhost:8000/

## 功能与特性

- 增量写入：若记录已存在则更新
- 重试机制：网络错误自动重试（最多10次）
- 分页浏览：每页20条，支持筛选
- 标记重要：可标记/取消标记问题
- Bootstrap 5 前端：卡片式布局

## 数据库

- 位置：`data/result.db`
- 表：`issues`
- 字段：issueid, project_name, summary, description, status, assignee_name, assignee_email, created, updated, issuetype, labels, priority, resolution, fixVersions, markdetail

## API 端点

| 端点 | 说明 |
|------|------|
| `GET /` | 前端首页（列表+筛选） |
| `GET /issue/<id>` | 问题详情页 |
| `POST /issue/<id>/toggle` | 切换标记状态 |
| `GET /issues` | REST API 列表 |
| `GET /issues/<id>` | REST API 详情 |

## 故障排查

- 网络问题、鉴权失败或 API 限流
- 检查凭据、网络连通性
- Jira 限流时会自动重试

## 许可证

许可证信息请参阅 LICENSE 文件