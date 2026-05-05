#!/usr/bin/env python3
"""Minimal REST API to expose issue details from the SQLite result DB.

Endpoints:
- GET /issues/<issueid> : detail of a single issue
- GET /issues          : list recent issues (read with limit param)
"""
import logging
from flask import Flask, jsonify, request, render_template, redirect
from db_writer import get_issue_by_id, query_results, query_results_paginated, query_results_paginated_filtered, update_issue_markdetail_field, get_marked_issues
from config import DEFAULT_LIMIT, MAX_LIMIT, CACHE_TTL
from cache import cache

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
logger = logging.getLogger(__name__)

def create_app():
    app = Flask(__name__)
    app.config['TEMPLATES_FOLDER'] = 'templates'

    def validate_limit_offset():
        try:
            limit = int(request.args.get('limit', DEFAULT_LIMIT))
            offset = int(request.args.get('offset', 0))
        except ValueError:
            limit = DEFAULT_LIMIT
            offset = 0
        limit = max(1, min(limit, MAX_LIMIT))
        offset = max(0, offset)
        return limit, offset

    def get_cache_key(prefix: str) -> str:
        return f"{prefix}:{request.full_path}"

    @app.before_request
    def before_request():
        cache.cleanup()

    @app.route('/')
    def index():
        cache_key = get_cache_key('index')
        cached = cache.get(cache_key)
        if cached:
            logger.info(f"Cache hit: {cache_key}")
            return cached

        page = request.args.get('page', 1, type=int)
        field = request.args.get('field', '')
        value = request.args.get('value', '')
        project = request.args.get('project', '')
        limit = 20
        offset = (page - 1) * limit

        if project:
            data = query_results_paginated_filtered(limit=limit, offset=offset, field='project_name', value=project)
            filter_display = f"项目 = {project}"
        elif field and value:
            data = query_results_paginated_filtered(limit=limit, offset=offset, field=field, value=value)
            filter_display = f"{field} = {value}"
        else:
            data = query_results_paginated(limit=limit, offset=offset)
            filter_display = ""

        total = data.get('total', 0)
        total_pages = max(1, (total + limit - 1) // limit)
        issues = data.get('issues', [])

        response = render_template('index.html',
            issues=issues,
            page=page,
            total=total,
            total_pages=total_pages,
            field=field,
            value=value,
            project=project,
            filter_display=filter_display)
        cache.set(cache_key, response, CACHE_TTL)
        return response

    @app.route('/issue/<issueid>')
    def issue_detail_page(issueid):
        cache_key = get_cache_key(f'issue_detail_page:{issueid}')
        cached = cache.get(cache_key)
        if cached:
            logger.info(f"Cache hit: {cache_key}")
            return cached

        issue = get_issue_by_id(issueid)
        if not issue:
            return "Issue not found", 404
        response = render_template('detail.html', issue=issue)
        cache.set(cache_key, response, CACHE_TTL)
        return response

    @app.route('/issue/<issueid>/toggle', methods=['POST'])
    def toggle_mark(issueid):
        issue = get_issue_by_id(issueid)
        if not issue:
            return "Issue not found", 404
        new_mark = '' if issue.get('markdetail') else 'Important'
        update_issue_markdetail_field(issueid, new_mark)
        cache.clear()
        return redirect(f'/issue/{issueid}')

    @app.route('/issues/<issueid>', methods=['GET'])
    def issue_detail(issueid):
        cache_key = get_cache_key(f'issue_detail:{issueid}')
        cached = cache.get(cache_key)
        if cached:
            logger.info(f"Cache hit: {cache_key}")
            return cached

        issue = get_issue_by_id(issueid)
        if not issue:
            return jsonify({'error': 'Not found'}), 404
        response = jsonify(issue)
        cache.set(cache_key, response, CACHE_TTL)
        return response

    @app.route('/issues/<issueid>/markdetail', methods=['POST'])
    def update_issue_markdetail_route(issueid):
        """Update the markdetail field for an issue."""
        markdetail = request.json.get('markdetail', '')
        success = update_issue_markdetail_field(issueid, markdetail)
        if success:
            cache.clear()
            return jsonify({'success': True, 'message': f'Markdetail updated for issue {issueid}'})
        return jsonify({'success': False, 'message': f'Failed to update markdetail for issue {issueid}'}), 500

    @app.route('/issues', methods=['GET'])
    def issues_list():
        cache_key = get_cache_key('issues_list')
        cached = cache.get(cache_key)
        if cached:
            logger.info(f"Cache hit: {cache_key}")
            return cached

        limit, offset = validate_limit_offset()
        field = request.args.get('field')
        value = request.args.get('value')
        if field and value is not None:
            data = query_results_paginated_filtered(limit=limit, offset=offset, field=field, value=value)
        else:
            data = query_results_paginated(limit=limit, offset=offset)
        response = jsonify(data)
        cache.set(cache_key, response, CACHE_TTL)
        return response

    @app.route('/marked')
    def marked_issues():
        page = request.args.get('page', 1, type=int)
        limit = 20
        offset = (page - 1) * limit
        data = get_marked_issues(limit=limit, offset=offset)
        total = data.get('total', 0)
        total_pages = max(1, (total + limit - 1) // limit)
        issues = data.get('issues', [])
        return render_template('index.html',
            issues=issues,
            page=page,
            total=total,
            total_pages=total_pages,
            field='',
            value='',
            project='',
            show_marked=True,
            filter_display=f"已标记 ({total} 条)")

    return app

app = create_app()

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8000)
