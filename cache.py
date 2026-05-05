"""简单内存缓存模块"""
import time
from typing import Any, Dict, Optional
from threading import Lock

class SimpleCache:
    def __init__(self, default_ttl: int = 60):
        self._cache: Dict[str, tuple] = {}
        self._ttl = default_ttl
        self._lock = Lock()

    def get(self, key: str) -> Optional[Any]:
        with self._lock:
            if key in self._cache:
                value, expires = self._cache[key]
                if time.time() < expires:
                    return value
                del self._cache[key]
        return None

    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> None:
        with self._lock:
            expires = time.time() + (ttl or self._ttl)
            self._cache[key] = (value, expires)

    def clear(self) -> None:
        with self._lock:
            self._cache.clear()

    def cleanup(self) -> None:
        with self._lock:
            now = time.time()
            expired = [k for k, (_, exp) in self._cache.items() if now >= exp]
            for k in expired:
                del self._cache[k]

cache = SimpleCache()