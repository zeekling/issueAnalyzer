# AGENTS 指南

本指南聚焦通用 Python 项目开发规范，避免对具体框架的依赖描述，确保跨框架复用性。

目标
- 提高工作流可重复性
- 降低认知成本
- 促进多人协作的一致性

Cursor与Copilot规则
- Cursor 规则: 未检测到本仓库内的 Cursor 规则（.cursor/rules/ 或 .cursorrules）。若后续有，请并入此处。
- Copilot 指令: 未检测到 .github/copilot-instructions.md。若后续存在，将相应要求纳入。

0) 自动修改策略
- 默认情况下，代理在你明确授权范围内，对仓库文件进行修改、注释添加、代码调整以及脚本更新等操作，不再单独请求额外确认。
- 如涉及潜在敏感信息或重大架构变更，将在提交前进行简要的自检并尽量说明变更原因。

1) 构建/Lint/测试命令
- 说明：以下命令优先使用 Python 生态，结合虚拟环境及常见依赖管理方案。

- 设置与依赖
  - Python 3.9+（推荐 Python 3.11+）
  - 虚拟环境：python -m venv .venv
  - 激活环境：
    Windows: .venv\\Scripts\\activate
    macOS/Linux: source .venv/bin/activate
  - 依赖管理：
    Poetry：poetry install
    也可本地：pip install -r requirements-dev.txt

- 构建与打包
  - 构建：python -m build
  - 或 Poetry：poetry build

- 依赖与环境锁定
  - Poetry：poetry lock

- 代码格式与静态分析
  - Black：用于格式化
  - isort：导入排序
  - Ruff：集合 lint/格式化/静态分析
  - Mypy：静态类型检查

- 测试
  - 测试框架：pytest 为主
  - 运行测试：pytest -q
  - 指定单元测试：
    pytest tests/<module>.py::TestClass::test_method -q
  - 运行某个测试文件：
    pytest tests/test_ui.py -q
  - 生成覆盖率：pytest --maxfail=1 --disable-warnings -q
    coverage run -m pytest
    coverage report -m

- 其他有用命令
  - pre-commit 安装与运行：pre-commit install && pre-commit run --all-files
  - 仅检查，非修复：ruff check .
  - 修复潜在问题：ruff check --fix .
  - 格式化代码：black . && isort .
  - 安全性检查：bandit -r .  //（如可用）

- 单一测试示例
  - 运行单个测试方法：pytest tests/test_ui.py::test_ui_endpoint -q
  - 运行带标签的测试集合：pytest -k "ui and not slow" -q

2) 代码风格指南
- 总体目标：清晰、可维护、可读性高

A) 导入与排序
- 标准库 > 第三方库 > 本地应用库
- 每组内按字母顺序排序
- 适当地使用空行分组
- 遵循 from X import Y 的分组排序，尽量避免大量星号导入

B) 代码格式化
- 使用 Black 进行格式化
- 使用 isort 处理导入排序
- 保存前自动或手动运行格式化检查，CI 也应强制

C) 类型与注释
- 强制类型注解，尤其函数签名和返回值
- 使用 from __future__ import annotations 以推迟评估
- 为公共接口编写 docstring，遵循 Google 或 NumPy 风格
- 对复杂逻辑给出简短注释，避免冗余注释

D) 命名约定
- 函数、方法、变量采用 snake_case
- 常量使用 ALL_CAPS，模块级别常量放在文件顶部
- 类名使用 CamelCase
- 接口和公共 API 使用清晰、描述性名称

E) 错误处理
- 尽量捕捉具体异常，避免 broad except
- 在不破坏调用方的前提下，抛出有意义的错误信息
- 使用自定义异常类以表达特定错误情境

F) 日志记录
- 使用 logging 模块，统一 logger 命名与级别
- 避免在生产日志中输出敏感信息
- 通过配置控制日志格式和输出

G) 文档与测试
- 每个函数、类必须有 docstring
- 关键逻辑应有自解释性，必要时添加类型注解
- 测试覆盖核心路径，避免对外部资源的强耦合
- 使用 fixtures/ factory 方法简化重复测试数据

H) Flask/模板
- 路由函数应简短，复杂逻辑提炼到服务层
- Jinja 模板应尽量简洁，避免复杂的逻辑
- 数据传递给模板使用显式变量，避免隐式全局

I) API设计
- 遵循一致的命名与路径结构
- 返回标准的 JSON/HTML 响应，带恰当的状态码和错误信息
- 参数校验放在请求入口，避免深层业务判断

J) 配置与环境
- 将配置暴露为环境变量，使用 pydantic or dynaconf 进行验证（按团队偏好）
- 将敏感信息排除在代码库之外
- 提供默认值与错误信息友好的提示

K) 版本与变更
- 变更日志要清晰，向后兼容性要写入文档
- 使用语义化版本号并在 PR 描述中说明

3) 维护与迭代
- 新语言/新框架加入时，扩展相应章节
- 脚手架/模板应保持向后兼容，逐步迁移
- 安全性审查作为常态化工作

4) 参考与链接
- 指向工具、库、规范的版本和文档
- 给出团队约定与落地示例链接

5) 历史表兼容性规则
- 当前项目默认无需考虑历史表兼容性
- 如后续需要引入历史表兼容，请在变更前明确规格和迁移策略，并在相关文档中以单独章节记录
- 变更风险：可能影响数据持久化、回滚、分析结果等。需要额外测试
- 启用开关：如需启用历史表兼容，建议通过配置项 HISTORY_TABLE_COMPATIBILITY=true/false 控制，默认 false
- 变更记录：每次启用/禁用历史表兼容性时，记录变更原因和影响范围

6) 提交策略
- 本地优先：所有生成的代码修改应仅在本地文件中进行，避免自动将内容提交到 git（包括暂存阶段）
- 变更确认：在进行实际提交前，需获得明确的确认（例如通过对话或任务指令中的“确认提交”步骤）
- 提交内容规范：提交时写清楚原因（why）以及变更内容（what），摘要1-2 行，详细描述放在正文中
- 避免敏感信息：提交前检查并排除敏感数据（如 .env、credentials.json、密钥、API token 等）。如必须提交，请先移除或通过环境变量注入后提交
- 不推送远端：不执行自动推送到远端分支，除非用户明确请求并确认
- 钩子与错误处理：若 pre-commit、lint、test 等钩子失败，需先修正后再提交。若已推送，原则上不要再对历史进行修改
- 工作流建议：变更 -> 本地测试/验证 -> 审阅/确认 -> 提交 -> 推送（可选）
- 通过此规则保障仓库历史的稳定性，便于追踪与回滚

Cursor rules and Copilot rules
- Cursor rules: None detected. If present, fold into this section.
- Copilot rules: None detected. If present, fold into this section.

结束。
