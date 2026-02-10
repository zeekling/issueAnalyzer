#!/usr/bin/env python3
"""PyWebIO front-end for Issue Browser with paginated listing (All Issues).

This frontend talks to the backend API endpoints:
- GET /issues?limit=N&offset=M
- GET /issues/{issueid}
It renders a paginated table of issues and allows viewing details of a single issue.
"""
from typing import List, Dict, Any, Optional, Tuple
from psycopg2 import NUMBER
import requests

from pywebio.input import input, select, input_group
from pywebio.output import put_html, put_table, put_text, clear, put_buttons
import re
from datetime import datetime
PAGINATION_CSS = """
<style>
.container { max-width: 2000px; min-width: 900px; }
.form-control {width: 200px;display: inline;}
.form-group { display: inline-block; width: 230px;}
</style>
"""


def fetch_issue(iid: str) -> Optional[Dict[str, Any]]:
    try:
        resp = requests.get(f"http://localhost:8000/issues/{iid}", timeout=5)
        if resp.ok:
            return resp.json()
    except Exception:
        return None
    return None


def fetch_issues_page(limit: int, offset: int, field: Optional[str] = None, value: Optional[str] = None) -> Tuple[int, List[Dict[str, Any]]]:
    try:
        params = {"limit": limit, "offset": offset}
        if field:
            params["field"] = field
        if value:
            params["value"] = value
        resp = requests.get("http://localhost:8000/issues", params=params, timeout=5)
        if resp.ok:
            data = resp.json()
            total = int(data.get("total", len(data.get("issues", []))))
            issues = data.get("issues", []) if isinstance(data, dict) else data
            if isinstance(issues, list):
                return total, issues
    except Exception:
        pass
    return 0, []


def display_issue(iid: str):
    issue = fetch_issue(iid)
    if not issue:
        put_html("<p>Issue not found or API error</p>")
        return
    put_html(render_issue_html(issue))


def render_issue_html(issue: Dict[str, Any]) -> str:
    issueid = issue.get("issueid", "")
    project_name = issue.get("project_name", "")
    summary = issue.get("summary", "")
    description = issue.get("description", "")
    status = issue.get("status", "")
    assignee = issue.get("assignee", {}) or {}
    name = assignee.get("name") if isinstance(assignee, dict) else None
    email = assignee.get("email") if isinstance(assignee, dict) else None
    created = issue.get("created", "")
    updated = issue.get("updated", "")
    issuetype = issue.get("issuetype", "")
    labels = ", ".join(issue.get("labels", [])) if isinstance(issue.get("labels", []), list) else ""
    priority = issue.get("priority", "")
    resolution = issue.get("resolution", "")
    fixVersions = ", ".join(issue.get("fixVersions", [])) if isinstance(issue.get("fixVersions", []), list) else ""
    created_at = issue.get("created_at", "")

    html = f"""
    <h2>Issue {issueid}: {summary}</h2>
    <p>{description}</p>
    <table border=1 cellpadding=4>
      <tr><td>Project</td><td>{project_name or ''}</td></tr>
      <tr><td>Status</td><td>{status}</td></tr>
      <tr><td>Assignee</td><td>{name or ''} &lt;{email or ''}&gt;</td></tr>
      <tr><td>Created</td><td>{created}</td></tr>
      <tr><td>Updated</td><td>{updated}</td></tr>
      <tr><td>Type</td><td>{issuetype}</td></tr>
      <tr><td>Labels</td><td>{labels}</td></tr>
      <tr><td>Priority</td><td>{priority}</td></tr>
      <tr><td>Resolution</td><td>{resolution}</td></tr>
      <tr><td>Fix Versions</td><td>{fixVersions}</td></tr>
      <tr><td>Created At</td><td>{created_at}</td></tr>
    </table>
    """
    return html


def _nav_onclick(btn_label):
    return btn_label

def mark_issue(iid: str):
    # Simple marker action for a given issue id
    put_text(f"Issue {iid} marked.")

def _format_date(val: Any) -> str:
    if val is None:
        return ""
    # numeric timestamp (s or ms)
    if isinstance(val, (int, float)):
        ts = float(val)
        if ts > 1e12:
            ts /= 1000.0
        try:
            dt = datetime.fromtimestamp(ts)
            return dt.date().strftime("%Y-%m-%d")
        except Exception:
            return ""
    # string representations
    if isinstance(val, str):
        s = val.strip()
        if not s:
            return ""
        if re.match(r'^\d{4}-\d{2}-\d{2}$', s):
            return s
        tmp = s
        if tmp.endswith("Z"):
            tmp = tmp[:-1] + "+00:00"
        try:
            dt = datetime.fromisoformat(tmp)
            return dt.date().strftime("%Y-%m-%d")
        except Exception:
            pass
        for fmt in ("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d"):
            try:
                dt = datetime.strptime(s, fmt)
                return dt.date().strftime("%Y-%m-%d")
            except Exception:
                pass
        # fallback to first 10 chars if possible
        return s[:10] if len(s) >= 10 else s
    return ""

def issue_change(iid: str):
    print(str)

def pywebio_ui():
    # Paginated listing with dropdown-based page navigation and field-based search
    page_size = 20
    page = 1
    # Persist search state across iterations
    search_field = ""
    search_value = ""
    field_options = [
        {'label': 'Issue ID', 'value': 'issueid'},
        {'label': 'Project', 'value': 'project_name'},
        {'label': 'Summary', 'value': 'summary'},
        {'label': 'Description', 'value': 'description'},
        {'label': 'Status', 'value': 'status'},
        {'label': 'Assignee', 'value': 'assignee_name'},
        {'label': 'Type', 'value': 'issuetype'},
        {'label': 'Labels', 'value': 'labels'},
        {'label': 'Priority', 'value': 'priority'},
        {'label': 'Resolution', 'value': 'resolution'},
        {'label': 'Created', 'value': 'created'},
        {'label': 'Updated', 'value': 'updated'},
    ]
    while True:
        # Read current search criteria from user inputs
        clear()
        put_html("<h1>Issue Browser (PyWebIO) - All Issues (Paged) with Filter</h1>")
        
        offset = (page - 1) * page_size
        total, issues = fetch_issues_page(page_size, offset, search_field, search_value)
        if not issues:
            put_text("No issues found or API unreachable.")
            break
        header = ["issueid", "project_name", "summary", "status", "issuetype", "fixVersions", "labels", "resolution", "created", "updated", "Operate"]
        rows = []
        for it in issues:
            iid = it.get("issueid", "")
            project_name = it.get("project_name", "")
            summary = it.get("summary", "")
            status = it.get("status", "")
            issuetype = it.get("issuetype", "")
            if not isinstance(issuetype, str):
                issuetype = str(issuetype)
            fixVersionsVal = it.get("fixVersions", [])
            fixVersions = ", ".join(fixVersionsVal) if isinstance(fixVersionsVal, list) else str(fixVersionsVal)
            labelsVal = it.get("labels", [])
            labels = ", ".join(labelsVal) if isinstance(labelsVal, list) else str(labelsVal)
            resolution = it.get("resolution", "")
            created = _format_date(it.get("created", ""))
            updated = _format_date(it.get("updated", ""))
            rows.append([iid, project_name, summary, status, issuetype, fixVersions, labels, resolution, created, updated, put_buttons(['Mark ', 'View'], onclick=lambda value, iid=iid: (mark_issue(iid) if value == 'Mark' else display_issue(iid)))])
        put_html(PAGINATION_CSS)
        put_table(rows, header=header)
        total_pages = max(1, (total + page_size - 1) // page_size)
        # Show current field-filter state near pagination
        filter_display = f"筛选: 字段={search_field}，值='{search_value}'" if search_value != "" else f"筛选: 字段={search_field}"
        put_html(f"<div>Page {page} of {total_pages} (size {page_size}) | {filter_display}</div>")
        # Set initial selected value for select
        select_value = None
        if search_field:
            for opt in field_options:
                if isinstance(opt, dict) and opt.get('value') == search_field:
                    select_value = opt
                    break
        search = input_group("Search ", [
            input("Go to page", name="page", type="number", value=page),
            select("search field", options=field_options, value=search_field, name="search_field"),
            input("Search value", name="search_value", value=select_value)
            ]);
        new_page_str=search["page"]
        selected_field = search["search_field"]
        search_field = selected_field.get('value') if isinstance(selected_field, dict) else selected_field
        search_value=search["search_value"]
        if new_page_str:
            new_page=int(new_page_str)
            if 1 <= new_page <= total_pages:
                page=new_page
        if not search_field or not search_value:
            search_field=""
            search_value=""
        
def main():
    pywebio_ui()
