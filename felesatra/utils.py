"""Shared utilities."""

import os
from urllib.parse import urljoin


def geturl(env, path):
    """Get the render URL for source file path."""
    if path.endswith('/'):
        dir_marker = '/'
    else:
        dir_marker = ''
    relpath = os.path.relpath(path, env.globals['site']['srcdir']) + dir_marker
    return urljoin(env.globals['site']['url'], relpath)
