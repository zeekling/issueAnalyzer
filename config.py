"""配置管理模块 - 所有配置集中在此文件"""
import os

# Jira 连接配置
MAX_RETRIES = int(os.environ.get("MAX_RETRIES", "10"))
RETRY_DELAY = int(os.environ.get("RETRY_DELAY", "3"))
REQUEST_INTERVAL = float(os.environ.get("REQUEST_INTERVAL", "1.0"))

# API 配置
DEFAULT_LIMIT = int(os.environ.get("DEFAULT_LIMIT", "20"))
MAX_LIMIT = int(os.environ.get("MAX_LIMIT", "1000"))

# 缓存配置
CACHE_TTL = int(os.environ.get("CACHE_TTL", "60"))

# 数据库配置
DB_PATH = os.environ.get("RESULTS_DB_PATH", "data/result.db")

# 日志配置
LOG_LEVEL = os.environ.get("LOG_LEVEL", "INFO")
LOG_FORMAT = "%(asctime)s %(levelname)s %(message)s"

# 跳过的问题类型
EXCLUDED_RESOLUTIONS = {
    "Won't Fix", "Duplicate", "Not A Problem", "Not A Bug",
    "Won't Do", "Invalid", "Delivered", "Abandoned",
    "Information Provided", "Incomplete"
}