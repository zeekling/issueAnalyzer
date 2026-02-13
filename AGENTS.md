# AGENTS 指南

Issue Analyzer Python 开发规范指南

## 项目概述

Jira Issue 抓取系统，提供 SQLite 存储、REST API 和 PyWebIO UI。核心文件：
- `jira_scraper.py` - Jira 数据抓取
- `db_writer.py` - SQLite 数据库操作
- `api.py` - Flask REST API
- `frontend/pywebio_app.py` - PyWebIO 前端

## 0) 自动修改策略

- 明确授权范围内修改文件无需单独确认
- 敏感信息或架构变更提交前自检说明原因

## 1) 构建/Lint/测试命令

**环境设置**
```bash
# Python 3.9+ (推荐 3.11+)
python -m venv .venv
# Windows: .venv\Scripts\activate
# Linux/macOS: source .venv/bin/activate
pip install -r requirements.txt
```

**依赖列表** (requirements.txt)
```
drf-yasg>=1.20.0
psycopg2-binary>=2.9
Pillow>=9.0.0
Whitenoise>=6.0
gunicorn>=20.0
requests>=2.28.1
Flask>=2.0
pywebio
```

**代码质量**
```bash
# 格式化
black .
isort .

# 静态检查
ruff check .
ruff check --fix .

# 类型检查
mypy .
```

**测试框架**
- pytest（未配置，需先创建测试文件）
- 单文件测试：`pytest tests/test_module.py -q`
- 单个测试：`pytest tests/test_module.py::TestClass::test_method -q`
- 完整测试：`pytest -q`

## 2) 代码风格

**导入顺序**（同目录优先）
1. 标准库导入
2. 第三方库导入
3. 本地应用导入
按字母顺序排序，适当分组，禁止星号导入

**格式化工具**
- Black（代码格式化）
- isort（导入排序）

**类型注解**
- 函数签名必须有类型注解
- 返回值必须注明类型
- 使用 `from __future__ import annotations`
- 示例：`def get_issue_by_id(issueid: str) -> Optional[Dict]`

**命名约定**
- 函数/方法/变量：snake_case
- 常量：ALL_CAPS
- 类：CamelCase

**文档字符串**
- 公共函数使用 Google 风格
- 示例：
```python
def store_result(input_id: str, data: Any) -> None:
    """Persist a single result record using separate fields."""
```

**错误处理**
- 捕捉具体异常，避免宽泛的 except
- 使用 logging 记录错误
- 自定义异常类（需要时）

**日志使用**
```python
import logging
logger = logging.getLogger(__name__)
logger.error("Failed to connect to database")
```

**Flask/数据库约定**
- 路由函数保持简洁，复杂逻辑抽离到服务层
- SQLite 并发使用 `threading.Lock` 保护
- API 返回标准 JSON，状态码正确（404/500 等）

## 3) Git 工作流

**分支管理**
- 禁止直接推送到 main/master
- 新功能必须创建独立分支
- 工作流：变更 → 新分支 → 测试 → 审阅 → PR → 合并

**提交规范**
- 摘要 1-2 行，原因放在正文
- 本地优先，仅提交确认的变更
- 避免提交敏感信息（.env、密钥）

**检查清单**
```bash
git status          # 查看未跟踪文件
git diff            # 查看变更内容
```

## 4) 开发流程

**数据库变更**
- 涉及表结构修改时添加迁移脚本
- 保持向后兼容（使用 IF NOT EXISTS）

**新增功能**
- 后端：更新 db_writer.py 和 api.py
- 前端：更新 pywebio_app.py
- 更新相关测试

**依赖管理**
- 新增依赖加入 requirements.txt
- 版本号使用 `>=` 确保兼容性

**安全性**
- 所有环境变量使用 `.env` 文件
- SQL 查询参数化，禁止字符串拼接
- 不在日志中记录敏感信息

## 5) 检查清单

开发前检查：
- [ ] 理解现有代码结构和约定
- [ ] 创建独立分支进行开发
- [ ] 编写或更新相关测试
- [ ] 运行 `black . && isort .`
- [ ] 运行 `ruff check .`
- [ ] 提交前确认变更内容
