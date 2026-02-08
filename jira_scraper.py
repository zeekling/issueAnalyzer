#!/usr/bin/env python3
import logging
import argparse
import time
import json
from typing import List, Dict, Any, Optional
import requests
from db_writer import init_db, store_result

DEFAULT_JIRA_BASE: str = "https://issues.apache.org/jira"
logger = logging.getLogger(__name__)

# Utilities for time-bounded JQL
def _date_literal(value: str) -> str:
    v = value.strip()
    if (v.startswith("'") and v.endswith("'")) or (v.startswith('"') and v.endswith('"')):
        return v
    # Use double quotes for Jira JQL literals
    return f'"{v}"'

def _build_time_jql(base_jql: str, field: str, start_date: Optional[str], end_date: Optional[str]) -> Optional[str]:
    if not start_date and not end_date:
        return None
    parts: List[str] = []
    if start_date:
        parts.append(f'{field} >= {_date_literal(start_date)}')
    if end_date:
        parts.append(f'{field} <= {_date_literal(end_date)}')
    clause = " AND ".join(parts)
    return f"{base_jql} AND {clause}"

def fetch_issues(jql: str, max_results: int = 1000, start_at: int = 0, auth: Any = None) -> Dict[str, Any]:
    """Fetch Jira issues using JQL and return raw data."""
    url = f"{DEFAULT_JIRA_BASE.rstrip('/')}/rest/api/2/search"
    params = {
        "jql": jql,
        "startAt": start_at,
        "maxResults": max_results,
        "fields": "key,summary,description,status,assignee,created,updated,issuetype,labels,priority,resolution,fixVersions",
    }
    try:
        resp = requests.get(url, params=params, auth=auth, timeout=60)
        resp.raise_for_status()
        data = resp.json()
        total = int(data.get("total", 0))
        issues = data.get("issues", [])
        logger.info("Fetched batch: %d issues (start_at=%d, max_results=%d) total=%d for JQL=%s", len(issues), start_at, max_results, total, jql)
        if issues:
            keys_preview = [iss.get("key") for iss in issues[:5]]
            preview = ", ".join(k for k in keys_preview if k)
            if len(issues) > 5:
                preview += ", ..."
            logger.debug("Batch issue keys: %s", preview)
        return {"total": total, "issues": issues}
    except Exception as e:
        logger.exception("Failed to fetch issues for JQL '%s' (start_at=%d, max_results=%d): %s", jql, start_at, max_results, e)
        raise

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
    """Normalize a Jira issue dict into a stable, serializable form."""
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
    parser.add_argument("--start-date", default="2017-01-01", help="Start date for time-bounded fetch (inclusive), format: YYYY-MM-DD")
    parser.add_argument("--end-date", default=None, help="End date for time-bounded fetch (inclusive), format: YYYY-MM-DD")
    parser.add_argument("--date-field", default="created", help="Date field to filter on (e.g., created, updated)")
    parser.add_argument("--max-results", type=int, default=1000, help="Pagination size per request")
    parser.add_argument("--username", default=None, help="Jira username for basic auth (optional)")
    parser.add_argument("--token", default=None, help="Jira API token for basic auth (optional)")
    args = parser.parse_args()

    # Initialize logging
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    init_db()
    base_jql = args.jql if args.jql else f"project = {args.project} AND resolution IS NOT EMPTY"
    # Build time-bounded JQL if dates provided
    if args.start_date or args.end_date:
        jql = _build_time_jql(base_jql, args.date_field or "created", args.start_date, args.end_date) or base_jql
    else:
        jql = base_jql
    auth = (args.username, args.token) if args.username and args.token else None
    logger.info("Using JQL: %s", jql)

    start_at = 0
    total = None
    total_to_store = 0
    while total is None or start_at < total:
        resp = fetch_issues(jql, max_results=args.max_results, start_at=start_at, auth=auth)
        total = resp["total"] if isinstance(resp, dict) else None
        issues = resp["issues"] if isinstance(resp, dict) else []
        if not issues:
            break
        # Process issues incrementally: normalize and upsert into DB
        normalized_batch = [normalize_issue(it) for it in issues]
        for it in normalized_batch:
            key = it.get("key")
            logger.info("Storing issue: %s", key)
            store_result(str(key), it)
            total_to_store += 1
        if total_to_store and total_to_store % 50 == 0:
            logger.info("Stored %d issues so far", total_to_store)
        start_at += len(issues)
        time.sleep(0.25)

if __name__ == "__main__":
    main()
