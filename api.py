#!/usr/bin/env python3
"""Minimal REST API to expose issue details from the SQLite result DB.

Endpoints:
- GET /issues/<issueid> : detail of a single issue
- GET /issues          : list recent issues (read with limit param)
"""
from flask import Flask, jsonify, request
from db_writer import get_issue_by_id, query_results

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
        except ValueError:
            limit = 100
        issues = query_results(limit=limit)
        return jsonify(issues)

    return app

if __name__ == '__main__':
    app = create_app()
    app.run(host='0.0.0.0', port=8000)
