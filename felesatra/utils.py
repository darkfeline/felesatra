"""Shared utilities."""

import functools
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


def cached_property(func):
    """Cached property."""
    cached = False
    value = None

    @functools.wraps(func)
    def get(self):
        # pylint: disable=missing-docstring
        nonlocal value
        nonlocal cached
        if not cached:
            value = func(self)
            cached = True
        return value

    def delete(self):
        # pylint: disable=missing-docstring,unused-argument
        nonlocal cached
        cached = False

    return property(fget=get, fdel=delete)
