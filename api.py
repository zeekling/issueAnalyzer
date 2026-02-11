#!/usr/bin/env python3
"""Minimal REST API to expose issue details from the SQLite result DB.

Endpoints:
- GET /issues/<issueid> : detail of a single issue
- GET /issues          : list recent issues (read with limit param)
"""
from flask import Flask, jsonify, request
from db_writer import get_issue_by_id, query_results, query_results_paginated, query_results_paginated_filtered, set_issue_important

def create_app():
    app = Flask(__name__)

    @app.route('/issues/<issueid>', methods=['GET'])
    def issue_detail(issueid):
        issue = get_issue_by_id(issueid)
        if not issue:
            return jsonify({'error': 'Not found'}), 404
        return jsonify(issue)

    @app.route('/issues', methods=['GET'])
    def issues_list():
        try:
            limit = int(request.args.get('limit', 100))
            offset = int(request.args.get('offset', 0))
        except ValueError:
            limit = 100
            offset = 0
        # Optional field-based filtering: ?field=<field>&value=<value>
        field = request.args.get('field')
        value = request.args.get('value')
        if field and value is not None:
            data = query_results_paginated_filtered(limit=limit, offset=offset, field=field, value=value)
        else:
            data = query_results_paginated(limit=limit, offset=offset)
        return jsonify(data)

    @app.route('/issues/<issueid>/important', methods=['POST'])
    def mark_issue_important(issueid):
        """Mark an issue as important."""
        is_important = request.json.get('is_important', True)
        success = set_issue_important(issueid, is_important)
        if success:
            return jsonify({'success': True, 'message': f'Issue {issueid} marked as {"important" if is_important else "not important"}'})
        return jsonify({'success': False, 'message': f'Failed to update issue {issueid}'}), 500

    @app.route('/issues/<issueid>/important', methods=['DELETE'])
    def clear_issue_important(issueid):
        """Clear the important mark from an issue."""
        success = set_issue_important(issueid, False)
        if success:
            return jsonify({'success': True, 'message': f'Important mark cleared from issue {issueid}'})
        return jsonify({'success': False, 'message': f'Failed to clear important mark for issue {issueid}'}), 500

    # PyWebIO front-end integrated on the same Flask app
    try:
        from frontend.pywebio_app import pywebio_ui
        from pywebio.platform.flask import webio_view
        app.add_url_rule('/ui', 'pywebio', webio_view(pywebio_ui), methods=['GET', 'POST', 'OPTIONS'])
    except Exception:
        # If PyWebIO dependencies are missing or import fails, skip integration gracefully
        pass

    return app

app = create_app()

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8000)
