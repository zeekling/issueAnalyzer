# AGENTS 指南

本指南聚焦 Issue Analyzer 项目的 Python 开发规范。

## 项目概述

Issue Analyzer 提供 Jira Issue 抓取、SQLite 存储、REST API 和 PyWebIO UI。核心文件：jira_scraper.py, db_writer.py, api.py, frontend/pywebio_app.py

## Cursor/Copilot 规则

- Cursor 规则: 未检测到 .cursor/rules/ 或 .cursorrules
- Copilot 规则: 未检测到 .github/copilot-instructions.md

## 0) 自动修改策略

- 代理在明确授权范围内修改文件、添加注释、调整代码，无需单独确认
- 涉及敏感信息或重大架构变更，提交前进行自检并说明原因

## 1) 构建/Lint/测试命令

**环境设置**
- Python 3.9+（推荐 3.11+）
- 虚拟环境：python -m venv .venv
- 激活：Windows (.venv\Scripts\activate), macOS/Linux (source .venv/bin/activate)
- 依赖：pip install -r requirements.txt

**代码质量**
- 格式化：black . && isort .
- 检查：ruff check .
- 修复：ruff check --fix .
- 静态类型：mypy .

**测试（无现有测试文件）**
- 测试框架：pytest（尚未配置）
- 运行所有测试：pytest -q
- 运行单文件：pytest tests/test_module.py -q
- 运行单个测试：pytest tests/test_module.py::TestClass::test_method -q

## 2) 代码风格

**导入**
- 标准库 > 第三方库 > 本地应用库
- 按字母顺序排序，适当分组
- 避免星号导入

**格式化**
- 使用 Black
- 使用 isort

**类型与注释**
- 强制类型注解（函数签名、返回值）
- 使用 from __future__ import annotations
- 公共接口写 docstring（Google 或 NumPy 风格）
- 复杂逻辑简短注释

**命名**
- 函数/方法/变量：snake_case
- 常量：ALL_CAPS
- 类：CamelCase

**错误处理**
- 捕捉具体异常，避免 broad except
- 抛出有意义的错误信息
- 使用自定义异常类

**日志**
- 使用 logging 模块
- 统一 logger 命名与级别
- 避免敏感信息

**Flask/数据库**
- 路由函数简短，复杂逻辑移服务层
- SQLite 使用 threading.Lock 保护并发
- API 返回标准 JSON，状态码正确

## 3) 提交策略

- 本地优先：代码仅修改本地文件
- 变更确认：提交前获得明确确认
- 提交规范：摘要 1-2 行，原因放在正文
- 避免敏感信息：检查 .env、密钥等
- 不推送远端：除非明确请求
- 工作流：变更 -> 测试验证 -> 审阅 -> 提交 -> 推送（可选）

## 4) 维护

- 新增依赖更新 requirements.txt
- 扩展字段需修改 jira_scraper.py 的解析逻辑
- 数据库变更需考虑迁移
- 安全性审查常态化
