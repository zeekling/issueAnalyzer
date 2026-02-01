#!/usr/bin/env python3
import argparse
import time
import json
from typing import List, Dict, Any
import requests
from db_writer import init_db, store_result

DEFAULT_JIRA_BASE = "https://issues.apache.org/jira"

def fetch_issues(jql: str, max_results: int = 1000, start_at: int = 0, auth: Any = None) -> Dict[str, Any]:
    url = f"{DEFAULT_JIRA_BASE.rstrip('/')}/rest/api/2/search"
    params = {
        "jql": jql,
        "startAt": start_at,
        "maxResults": max_results,
        "fields": "key,summary,description,status,assignee,created,updated,issuetype,labels,priority,resolution,fixVersions",
    }
    resp = requests.get(url, params=params, auth=auth, timeout=60)
    resp.raise_for_status()
    data = resp.json()
    return {"total": int(data.get("total", 0)), "issues": data.get("issues", [])}

def _extract_description_text(desc: Any) -> str:
    if isinstance(desc, str):
        return desc
    if isinstance(desc, list):
        return " ".join([_extract_description_text(d) for d in desc if d is not None])
    if isinstance(desc, dict):
        parts = []
        for v in desc.values():
            parts.append(_extract_description_text(v))
        return " ".join([p for p in parts if p])
    return str(desc)

def normalize_issue(issue: Dict[str, Any]) -> Dict[str, Any]:
    key = issue.get("key")
    fields = issue.get("fields", {})
    summary = fields.get("summary")
    status = fields.get("status", {}).get("name") if isinstance(fields.get("status"), dict) else None
    assignee = fields.get("assignee") or {}
    assignee_name = assignee.get("displayName") if isinstance(assignee, dict) else None
    assignee_email = assignee.get("emailAddress") if isinstance(assignee, dict) else None
    created = fields.get("created")
    updated = fields.get("updated")
    issuetype = fields.get("issuetype", {}).get("name") if isinstance(fields.get("issuetype"), dict) else None
    labels = fields.get("labels", []) or []
    priority = fields.get("priority", {}).get("name") if isinstance(fields.get("priority"), dict) else None
    resolution = fields.get("resolution", {}).get("name") if isinstance(fields.get("resolution"), dict) else None
    fixVersions = [fv.get("name") for fv in fields.get("fixVersions", []) if isinstance(fv, dict) and fv.get("name")]
    description_desc = fields.get("description")
    description = _extract_description_text(description_desc)
    return {
        "key": key,
        "summary": summary,
        "description": description,
        "status": status,
        "assignee": {"name": assignee_name, "email": assignee_email},
        "created": created,
        "updated": updated,
        "issuetype": issuetype,
        "labels": labels,
        "priority": priority,
        "resolution": resolution,
        "fixVersions": fixVersions,
    }

def main():
    parser = argparse.ArgumentParser(description="Fetch Jira issues for a given project from Apache Jira.")
    parser.add_argument("--project", default="YARN", help="Jira project key")
    parser.add_argument("--jql", default=None, help="Custom JQL, overrides --project if provided")
    parser.add_argument("--max-results", type=int, default=1000, help="Pagination size per request")
    parser.add_argument("--username", default=None, help="Jira username for basic auth (optional)")
    parser.add_argument("--token", default=None, help="Jira API token for basic auth (optional)")
    args = parser.parse_args()

    init_db()
    jql = args.jql if args.jql else f"project = {args.project} AND resolution IS NOT EMPTY"
    auth = (args.username, args.token) if args.username and args.token else None

    all_raw: List[Dict[str, Any]] = []
    start_at = 0
    total = None
    while total is None or start_at < total:
        resp = fetch_issues(jql, max_results=args.max_results, start_at=start_at, auth=auth)
        total = resp["total"] if isinstance(resp, dict) else None
        issues = resp["issues"] if isinstance(resp, dict) else []
        all_raw.extend(issues)
        start_at += len(issues)
        if not issues:
            break
        time.sleep(0.25)

    normalized = [normalize_issue(it) for it in all_raw]
    for it in normalized:
        store_result(str(it.get("key")), it)

if __name__ == "__main__":
    main()
