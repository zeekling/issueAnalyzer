"""
db_writer.py - SQLite-based storage for Jira analysis results.
"""
import os
import json
import sqlite3
import threading

# Default database path, can be overridden via env VAR 'RESULTS_DB_PATH'
DB_PATH = os.environ.get("RESULTS_DB_PATH", "results.db")
_lock = threading.Lock()

def init_db(db_path: str = None):
    global DB_PATH
    if db_path:
        DB_PATH = db_path
    os.makedirs(os.path.dirname(DB_PATH) or ".", exist_ok=True)
    with sqlite3.connect(DB_PATH) as conn:
        c = conn.cursor()
        c.execute(
            """
            CREATE TABLE IF NOT EXISTS results (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                input_id TEXT,
                data TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )
        conn.commit()

def store_result(input_id: str, data):
    """ Persist a single result record. Data will be serialized to JSON. """
    with _lock, sqlite3.connect(DB_PATH) as conn:
        c = conn.cursor()
        c.execute("INSERT INTO results (input_id, data) VALUES (?, ?)",
                  (input_id, json.dumps(data, ensure_ascii=False)))
        conn.commit()

def query_results(limit: int = 100):
    """ Query recent results; returns a list of dicts. """
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute("SELECT id, input_id, data, created_at FROM results ORDER BY id DESC LIMIT ?", (limit,))
        rows = cur.fetchall()
    return [
        {"id": r[0], "input_id": r[1], "data": json.loads(r[2]), "created_at": r[3]} for r in rows
    ]

if __name__ == "__main__":
    init_db()
    store_result("demo", {"example": True})
    print(query_results(5))
