# Issue Analyzer 使用说明

本仓库提供从 Apache Jira 的 YARN 项目抓取、存储、查询和浏览 Jira issues 的工具链。核心组件包括：
- jira_scraper.py：从 Jira 抓取 Issue，并写入本地 SQLite 数据库
- db_writer.py：SQLite 存储与查询引擎
- api.py：REST API，提供问题详情和分页查询
- frontend/pywebio_app.py：PyWebIO 前端 UI，提供分页浏览与详情查看

数据存储与端点
- 数据库位置：data/result.db
- 表：issues，字段包含
  - issueid, project_name, summary, description, status
  - assignee_name, assignee_email, created, updated
  - issuetype, labels, priority, resolution, fixVersions
  - created_at

快速开始
- 依赖
  - Python 3.x
  - requests
  - 其他依赖见 requirements.txt
- 环境与安装
  1) 创建并激活虚拟环境
  ```bash
  python3 -m venv .venv
  source .venv/bin/activate  # macOS/Linux
  # Windows:
  .\\venv\\Scripts\\activate
  ```
- 安装依赖
  ```bash
  pip install -U pip
  pip install -r requirements.txt
  ```
- 运行 Jira 抓取脚本
  ```bash
  python jira_scraper.py --project YARN
  ```
- 运行 API 服务
  ```bash
  python api.py
  ```
- 运行 PyWebIO UI
  打开浏览器访问 http://localhost:8000/ui

功能与特性
- 增量写入：Jira 抓取改为增量写入数据库，若记录已存在则更新
- 新增字段 project_name：记录 Issue 所属的 Jira 项目名称
- API 支持分页与按字段筛选：/issues?limit=&offset=&field=&value=
- PyWebIO UI：分页浏览并显示当前筛选状态，查看单条 Issue 不阻塞筛选

数据模型与 API 端点
- 数据模型：issues(issueid, project_name, summary, description, status, assignee_name, assignee_email, created, updated, issuetype, labels, priority, resolution, fixVersions, created_at)
- API 端点
  - GET /issues?limit=N&offset=M
  - GET /issues/{issueid}
  - GET /issues?field=X&value=Y（可选，带筛选）

前端 UI 使用
- 路由 /ui 提供 PyWebIO 界面，后端端点仍然是 /issues 与 /issues/{issueid}
- UI 通过接口获取数据，显示分页列表与单条详情

维护与扩展
- 如需新增依赖，请同步更新 requirements.txt
- 如需扩展输出字段，请修改 jira_scraper.py 的字段定义与 normalize_issue 的解析逻辑
- 若要增强筛选，扩展 query_results_paginated_filtered，并更新前端

故障排查
- 网络问题、鉴权失败或 API 限流等
- 检查凭据、网络连通性以及 Jira 实例配置

版本历史
- 1.0.0：初始版本，包含 Jira 抓取、数据库、API、以及前端 UI
- 1.1.0：新增 project_name 字段、增量写入、筛选 API、UI 显示筛选状态

许可证
- 许可证信息请参阅 LICENSE 文件，若无请联系维护者

关注与贡献
- 欢迎提交 PR，请遵循团队的贡献规范
