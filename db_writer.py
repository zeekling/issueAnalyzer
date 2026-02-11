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
                is_important INTEGER DEFAULT 0,
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
                labels, priority, resolution, fixVersions
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                issueid, project_name, summary, description, status,
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
                issueid, project_name, summary, description, status,
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
            "project_name": r[1],
            "summary": r[2],
            "description": r[3],
            "status": r[4],
            "assignee_name": r[5],
            "assignee_email": r[6],
            "created": r[7],
            "updated": r[8],
            "issuetype": r[9],
            "labels": json.loads(r[10]),
            "priority": r[11],
            "resolution": r[12],
            "fixVersions": json.loads(r[13]),
            "is_important": r[14] == 1,
            "created_at": r[15],
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
                issueid, project_name, summary, description, status,
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
            "project_name": r[1],
            "summary": r[2],
            "description": r[3],
            "status": r[4],
            "assignee_name": r[5],
            "assignee_email": r[6],
            "created": r[7],
            "updated": r[8],
            "issuetype": r[9],
            "labels": json.loads(r[10]),
            "priority": r[11],
            "resolution": r[12],
            "fixVersions": json.loads(r[13]),
            "is_important": r[14] == 1,
            "created_at": r[15],
        })
    return {"total": total, "issues": issues}

def query_results_paginated_filtered(limit: int = 100, offset: int = 0, field: Optional[str] = None, value: Optional[str] = None) -> Dict[str, Any]:
    """Return paginated results with simple field-based filtering.

    Supports filtering by a subset of fields using LIKE semantics.
    For labels/fixVersions stored as JSON arrays, performs a contains-style match.
    """
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        total = 0
        issues: List[Dict[str, Any]] = []
        if field:
            # Map user-provided field to DB column
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
                # Build LIKE pattern
                if column in ('labels', 'fixVersions'):
                    pattern = f'%"{value}"%'
                else:
                    pattern = f'%{value}%' if value is not None else ''
                if value is None:
                    # No value to filter on; fall back to unfiltered paging
                    column = None
                else:
                    # Count total with filter
                    cur.execute(
                        f"SELECT COUNT(*) FROM issues WHERE {column} LIKE ?",
                        (pattern,),
                    )
                    total_row = cur.fetchone()
                    total = total_row[0] if total_row else 0
                    # Fetch page with filter
                    sql = f"""
                    SELECT
                        issueid, project_name, summary, description, status,
                        assignee_name, assignee_email, created, updated, issuetype,
                        labels, priority, resolution, fixVersions, created_at
                    FROM issues
                    WHERE {column} LIKE ?
                    ORDER BY created_at DESC
                    LIMIT ? OFFSET ?
                    """
                    cur.execute(sql, (pattern, limit, offset))
                    rows = cur.fetchall()
                    for r in rows:
                        issues.append({
                            'issueid': r[0],
                            'project_name': r[1],
                            'summary': r[2],
                            'description': r[3],
                            'status': r[4],
                            'assignee': {'name': r[5], 'email': r[6]},
                            'created': r[7],
                            'updated': r[8],
                            'issuetype': r[9],
                            'labels': json.loads(r[10]),
                            'priority': r[11],
                            'resolution': r[12],
                            'fixVersions': json.loads(r[13]),
                            'is_important': r[14] == 1,
                            'created_at': r[15],
                        })
        # If there was no valid filter, fallback to unfiltered paging
        if not field or not issues:
            # Fall back to unfiltered paging
            return query_results_paginated(limit=limit, offset=offset)
        # Build total value already computed above; ensure non-negative
        total = total if isinstance(total, int) else 0
        return {"total": total, "issues": issues}

def get_issue_by_id(issueid: str) -> Optional[Dict[str, Any]]:
    """Fetch a single issue by its issueid."""
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute(
            """
            SELECT
                issueid, project_name, summary, description, status,
                assignee_name, assignee_email, created, updated, issuetype,
                labels, priority, resolution, fixVersions, is_important, created_at
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
        "project_name": row[1],
        "summary": row[2],
        "description": row[3],
        "status": row[4],
        "assignee": {"name": row[5], "email": row[6]},
        "created": row[7],
        "updated": row[8],
        "issuetype": row[9],
        "labels": json.loads(row[10]),
        "priority": row[11],
        "resolution": row[12],
        "fixVersions": json.loads(row[13]),
        "is_important": row[14] == 1,
        "created_at": row[15],
    }

def set_issue_important(issueid: str, is_important: bool) -> bool:
    """Set or clear the important status of an issue."""
    with sqlite3.connect(DB_PATH) as conn:
        cur = conn.cursor()
        cur.execute(
            """
            UPDATE issues SET is_important = ? WHERE issueid = ?
            """,
            (1 if is_important else 0, issueid),
        )
        conn.commit()
        return cur.rowcount > 0

if __name__ == "__main__":
    init_db()
    store_result("demo", {"example": True})
    print(query_results(5))
