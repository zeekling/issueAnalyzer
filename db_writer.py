"""
db_writer.py - SQLite-based storage for Jira analysis results.
"""
import json
import os
import sqlite3
import threading
from typing import Any, Optional, List, Dict

# Default database path, can be overridden via env VAR 'RESULTS_DB_PATH'
DB_PATH: str = os.environ.get("RESULTS_DB_PATH", "data/result.db")
_lock = threading.Lock()

def init_db(db_path: Optional[str] = None) -> None:
    global DB_PATH
    if db_path:
        DB_PATH = db_path
    os.makedirs(os.path.dirname(DB_PATH) or ".", exist_ok=True)
    with sqlite3.connect(DB_PATH) as conn:
        c = conn.cursor()
        c.execute("DROP TABLE IF EXISTS issues")
        c.execute(
            """
            CREATE TABLE IF NOT EXISTS issues (
                issueid TEXT PRIMARY KEY,
                summary TEXT,
                description TEXT,
                status TEXT,
                assignee_name TEXT,
                assignee_email TEXT,
                created TEXT,
                updated TEXT,
                issuetype TEXT,
                labels TEXT,
                priority TEXT,
                resolution TEXT,
                fixVersions TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )
        conn.commit()

def store_result(input_id: str, data: Any) -> None:
    """ Persist a single result record using separate fields. """
    with _lock, sqlite3.connect(DB_PATH) as conn:
        c = conn.cursor()
        # Derive fields from normalized issue dict
        issueid = None
        summary = None
        description = None
        status = None
        assignee_name = None
        assignee_email = None
        created = None
        updated = None
        issuetype = None
        labels = []
        priority = None
        resolution = None
        fixVersions = []
        if isinstance(data, dict):
            issueid = data.get("key")
            summary = data.get("summary")
            description = data.get("description")
            status = data.get("status")
            assignee = data.get("assignee")
            if isinstance(assignee, dict):
                assignee_name = assignee.get("name")
                assignee_email = assignee.get("email")
            created = data.get("created")
            updated = data.get("updated")
            issuetype = data.get("issuetype")
            labels = data.get("labels") or []
            priority = data.get("priority")
            resolution = data.get("resolution")
            fixVersions = data.get("fixVersions") or []
        labels_json = json.dumps(labels, ensure_ascii=False)
        fixVersions_json = json.dumps(fixVersions, ensure_ascii=False)
        c.execute(
            """
            INSERT OR REPLACE INTO issues (
                issueid, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                issueid, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels_json, priority, resolution, fixVersions_json
            ),
        )
        conn.commit()

def query_results(limit: int = 100) -> List[Dict[str, Any]]:
    """ Query recent issues; returns a list of dicts with separate fields. """
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute(
            """
            SELECT
                issueid, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, created_at
            FROM issues
            ORDER BY created_at DESC
            LIMIT ?
            """,
            (limit,),
        )
        rows = cur.fetchall()
    results = []
    for r in rows:
        results.append({
            "issueid": r[0],
            "summary": r[1],
            "description": r[2],
            "status": r[3],
            "assignee_name": r[4],
            "assignee_email": r[5],
            "created": r[6],
            "updated": r[7],
            "issuetype": r[8],
            "labels": json.loads(r[9]),
            "priority": r[10],
            "resolution": r[11],
            "fixVersions": json.loads(r[12]),
            "created_at": r[13],
        })
    return results

def query_results_paginated(limit: int = 100, offset: int = 0) -> Dict[str, Any]:
    """Return paginated results: total and list of issues for the current page."""
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        # total count
        cur.execute("SELECT COUNT(*) FROM issues")
        total_row = cur.fetchone()
        total = total_row[0] if total_row else 0
        # fetch page
        cur.execute(
            """
            SELECT
                issueid, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, created_at
            FROM issues
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
            """,
            (limit, offset),
        )
        rows = cur.fetchall()
    issues = []
    for r in rows:
        issues.append({
            "issueid": r[0],
            "summary": r[1],
            "description": r[2],
            "status": r[3],
            "assignee_name": r[4],
            "assignee_email": r[5],
            "created": r[6],
            "updated": r[7],
            "issuetype": r[8],
            "labels": json.loads(r[9]),
            "priority": r[10],
            "resolution": r[11],
            "fixVersions": json.loads(r[12]),
            "created_at": r[13],
        })
    return {"total": total, "issues": issues}

def get_issue_by_id(issueid: str) -> Optional[Dict[str, Any]]:
    """Fetch a single issue by its issueid."""
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute(
            """
            SELECT
                issueid, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, created_at
            FROM issues
            WHERE issueid = ?
            """,
            (issueid,),
        )
        row = cur.fetchone()
    if not row:
        return None
    return {
        "issueid": row[0],
        "summary": row[1],
        "description": row[2],
        "status": row[3],
        "assignee": {"name": row[4], "email": row[5]},
        "created": row[6],
        "updated": row[7],
        "issuetype": row[8],
        "labels": json.loads(row[9]),
        "priority": row[10],
        "resolution": row[11],
        "fixVersions": json.loads(row[12]),
        "created_at": row[13],
    }

if __name__ == "__main__":
    init_db()
    store_result("demo", {"example": True})
    print(query_results(5))
