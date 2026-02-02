#!/usr/bin/env python3
"""PyWebIO front-end for Issue Browser with paginated listing (All Issues).

This frontend talks to the backend API endpoints:
- GET /issues?limit=N&offset=M
- GET /issues/{issueid}
It renders a paginated table of issues and allows viewing details of a single issue.
"""
from typing import List, Dict, Any, Optional, Tuple
import requests

from pywebio.platform.flask import webio_view
from pywebio.input import input
from pywebio.output import put_text, put_html, put_table, put_buttons, clear


def fetch_issue(iid: str) -> Optional[Dict[str, Any]]:
    try:
        resp = requests.get(f"http://localhost:8000/issues/{iid}", timeout=5)
        if resp.ok:
            return resp.json()
    except Exception:
        return None
    return None


def fetch_issues_page(limit: int, offset: int) -> Tuple[int, List[Dict[str, Any]]]:
    try:
        resp = requests.get("http://localhost:8000/issues", params={"limit": limit, "offset": offset}, timeout=5)
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


def pywebio_ui():
    # Paginated list with navigation controls
    page_size = 20
    page = 1
    while True:
        offset = (page - 1) * page_size
        total, issues = fetch_issues_page(page_size, offset)
        clear()
        put_html("<h1>Issue Browser (PyWebIO) - All Issues (Paged)</h1>")
        if not issues:
            put_text("No issues found or API unreachable.")
            break
        header = ["issueid", "summary", "status", "assignee", "created", "updated"]
        rows = []
        for it in issues:
            iid = it.get("issueid", "")
            summary = it.get("summary", "")
            status = it.get("status", "")
            assignee = it.get("assignee", {}) or {}
            name = assignee.get("name") if isinstance(assignee, dict) else None
            email = assignee.get("email") if isinstance(assignee, dict) else None
            created = it.get("created")
            updated = it.get("updated")
            assignee_str = f"{name} <{email}>" if name or email else ""
            rows.append([iid, summary, status, assignee_str, created, updated])
        put_table(rows, header=header)
        total_pages = max(1, (total + page_size - 1) // page_size)
        put_html(f"<div>Page {page} of {total_pages} (size {page_size})</div>")
        nav = input("Navigate: Prev(N), Next(P), or Go to page (G), or page number (enter) and open issue by id (I):")
        if not nav:
            break
        tn = str(nav).strip()
        if tn.upper() == 'N' or tn.lower() == 'next':
            if page < total_pages:
                page += 1
                continue
        if tn.upper() == 'P' or tn.lower() == 'prev':
            if page > 1:
                page -= 1
                continue
        if tn.upper() == 'G' or tn.lower() == 'goto':
            p = input("Enter page number:")
            if p and p.isdigit():
                pnum = int(p)
                if 1 <= pnum <= total_pages:
                    page = pnum
                    continue
        if tn.isdigit():
            pnum = int(tn)
            if 1 <= pnum <= total_pages:
                page = pnum
                continue
        # If user inputs an issue id directly, view it
        display_issue(tn)
