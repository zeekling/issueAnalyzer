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

def _row_to_issue(row: tuple) -> Dict[str, Any]:
    """将数据库行转换为 issue 字典"""
    return {
        "issueid": row[0],
        "project_name": row[1],
        "summary": row[2],
        "description": row[3],
        "status": row[4],
        "assignee": {"name": row[5], "email": row[6]},
        "created": row[7],
        "updated": row[8],
        "issuetype": row[9],
        "labels": json.loads(row[10]) if row[10] else [],
        "priority": row[11],
        "resolution": row[12],
        "fixVersions": json.loads(row[13]) if row[13] else [],
        "markdetail": row[14],
    }

def init_db(db_path: Optional[str] = None) -> None:
    global DB_PATH
    if db_path:
        DB_PATH = db_path
    os.makedirs(os.path.dirname(DB_PATH) or ".", exist_ok=True)
    with sqlite3.connect(DB_PATH) as conn:
        c = conn.cursor()
        # Create table with project_name column if it does not exist
        c.execute(
            """
            CREATE TABLE IF NOT EXISTS issues (
                issueid TEXT PRIMARY KEY,
                project_name TEXT,
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
                markdetail TEXT DEFAULT ''
            )
            """
        )
        conn.execute("PRAGMA journal_mode=WAL")
        c.execute("CREATE INDEX IF NOT EXISTS idx_project_name ON issues(project_name)")
        c.execute("CREATE INDEX IF NOT EXISTS idx_status ON issues(status)")
        c.execute("CREATE INDEX IF NOT EXISTS idx_created ON issues(created)")
        conn.commit()

def store_result(input_id: str, data: Any) -> None:
    """ Persist a single result record using separate fields. """
    with _lock, sqlite3.connect(DB_PATH) as conn:
        c = conn.cursor()
        # Derive fields from normalized issue dict
        issueid = None
        project_name = None
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
            project_name = data.get("project_name")
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
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, markdetail
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels_json, priority, resolution, fixVersions_json, ''
            ),
        )
        conn.commit()

def store_results_batch(data_list: List[Dict[str, Any]]) -> int:
    """批量写入多个 issue，返回写入数量"""
    if not data_list:
        return 0
    with _lock, sqlite3.connect(DB_PATH) as conn:
        c = conn.cursor()
        records = []
        for data in data_list:
            issueid = data.get("key")
            summary = data.get("summary")
            description = data.get("description")
            status = data.get("status")
            assignee = data.get("assignee", {})
            assignee_name = assignee.get("name") if isinstance(assignee, dict) else None
            assignee_email = assignee.get("email") if isinstance(assignee, dict) else None
            project_name = data.get("project_name")
            created = data.get("created")
            updated = data.get("updated")
            issuetype = data.get("issuetype")
            labels = data.get("labels") or []
            priority = data.get("priority")
            resolution = data.get("resolution")
            fixVersions = data.get("fixVersions") or []
            records.append((
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                json.dumps(labels, ensure_ascii=False), priority, resolution,
                json.dumps(fixVersions, ensure_ascii=False), ''
            ))
        c.executemany(
            """INSERT OR REPLACE INTO issues (
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, markdetail
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
            records
        )
        conn.commit()
        return len(records)

def query_results(limit: int = 100) -> List[Dict[str, Any]]:
    """ Query recent issues; returns a list of dicts with separate fields. """
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute(
            """SELECT
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, markdetail
            FROM issues
            ORDER BY created DESC
            LIMIT ?""",
            (limit,),
        )
        rows = cur.fetchall()
    return [_row_to_issue(r) for r in rows]

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
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, markdetail
            FROM issues
            ORDER BY created DESC
            LIMIT ? OFFSET ?""",
            (limit, offset),
        )
        rows = cur.fetchall()
    return {"total": total, "issues": [_row_to_issue(r) for r in rows]}

def query_results_paginated_filtered(limit: int = 100, offset: int = 0, field: Optional[str] = None, value: Optional[str] = None) -> Dict[str, Any]:
    """Return paginated results with simple field-based filtering.

    Supports filtering by a subset of fields using LIKE semantics.
    For labels/fixVersions stored as JSON arrays, performs a contains-style match.
    """
    VALID_FIELDS = {'issueid', 'project_name', 'summary', 'description', 'status',
                    'assignee_name', 'issuetype', 'labels', 'priority', 'resolution',
                    'fixVersions', 'created', 'updated'}
    if field and field not in VALID_FIELDS:
        field = None
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        total = 0
        issues: List[Dict[str, Any]] = []
        if field:
            field_map = {
                'issueid': 'issueid',
                'project_name': 'project_name',
                'summary': 'summary',
                'description': 'description',
                'status': 'status',
                'assignee': 'assignee_name',
                'assignee_name': 'assignee_name',
                'issuetype': 'issuetype',
                'labels': 'labels',
                'priority': 'priority',
                'resolution': 'resolution',
                'fixVersions': 'fixVersions',
                'created': 'created',
                'updated': 'updated',
            }
            column = field_map.get(field)
            if column:
                if column in ('labels', 'fixVersions'):
                    pattern = f'%"{value}"%'
                else:
                    pattern = f'%{value}%' if value is not None else ''
                if value is None:
                    column = None
                else:
                    cur.execute(
                        f"SELECT COUNT(*) FROM issues WHERE {column} LIKE ?",
                        (pattern,),
                    )
                    total_row = cur.fetchone()
                    total = total_row[0] if total_row else 0
                    sql = f"""SELECT
                        issueid, project_name, summary, description, status,
                        assignee_name, assignee_email, created, updated, issuetype,
                        labels, priority, resolution, fixVersions, markdetail
                    FROM issues
                    WHERE {column} LIKE ?
                    ORDER BY created DESC
                    LIMIT ? OFFSET ?"""
                    cur.execute(sql, (pattern, limit, offset))
                    rows = cur.fetchall()
                    issues = [_row_to_issue(r) for r in rows]
        if not field or not issues:
            return query_results_paginated(limit=limit, offset=offset)
        total = total if isinstance(total, int) else 0
        return {"total": total, "issues": issues}

def get_issue_by_id(issueid: str) -> Optional[Dict[str, Any]]:
    """Fetch a single issue by its issueid."""
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute(
            """SELECT
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, markdetail
            FROM issues
            WHERE issueid = ?""",
            (issueid,),
        )
        row = cur.fetchone()
    if not row:
        return None
    return _row_to_issue(row)

def get_marked_issues(limit: int = 100, offset: int = 0) -> Dict[str, Any]:
    """获取所有已标记的 issues"""
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute("SELECT COUNT(*) FROM issues WHERE markdetail IS NOT NULL AND markdetail != ''")
        total = cur.fetchone()[0] or 0
        cur.execute(
            """SELECT issueid, project_name, summary, description, status,
               assignee_name, assignee_email, created, updated, issuetype,
               labels, priority, resolution, fixVersions, markdetail
            FROM issues
            WHERE markdetail IS NOT NULL AND markdetail != ''
            ORDER BY updated DESC
            LIMIT ? OFFSET ?""",
            (limit, offset),
        )
        rows = cur.fetchall()
    return {"total": total, "issues": [_row_to_issue(r) for r in rows]}

def update_issue_markdetail_field(issueid: str, markdetail: str) -> bool:
    """Update the markdetail field for an issue."""
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute(
            """
            UPDATE issues SET markdetail = ? WHERE issueid = ?
            """,
            (markdetail, issueid),
        )
        conn.commit()
        return cur.rowcount > 0

if __name__ == "__main__":
    init_db()
    store_result("demo", {"example": True})
    print(query_results(5))
