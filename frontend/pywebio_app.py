#!/usr/bin/env python3
"""PyWebIO front-end for Issue Browser (AJAX-driven).

This app uses PyWebIO integrated with Flask to provide a simple web UI
that interacts with the backend API endpoints:
- GET /issues/{issueid}
- GET /issues?limit=N
"""
from typing import List, Dict, Any, Optional
import requests

from pywebio.platform.flask import webio_view
from pywebio.input import input
from pywebio.output import put_text, put_html, put_buttons

def fetch_issue(issueid: str) -> Optional[Dict[str, Any]]:
    try:
        resp = requests.get(f"http://localhost:8000/issues/{issueid}", timeout=5)
        if resp.ok:
            return resp.json()
    except Exception:
        return None
    return None

def fetch_recent(limit: int = 5) -> List[Dict[str, Any]]:
    try:
        resp = requests.get(f"http://localhost:8000/issues?limit={limit}", timeout=5)
        if resp.ok:
            return resp.json()
    except Exception:
        return []
    return []

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

def display_issue(iid: str):
    issue = fetch_issue(iid)
    if not issue:
        put_html("<p>Issue not found or API error</p>")
        return
    put_html(render_issue_html(issue))

def pywebio_ui():
    put_html("<h1>Issue Browser (PyWebIO)</h1>")
    # Load single issue by user input
    iid = input("Enter issue id")
    if iid:
        display_issue(str(iid))
    # Show recent issues as quick access buttons
    recent = fetch_recent(5)
    if recent:
        ids = [str(it.get("issueid")) for it in recent if it.get("issueid")]
        if ids:
            put_html("<hr/>")
            put_html("<div>Recent Issues:</div>")
            put_buttons(ids, onclick=lambda v: display_issue(v))
