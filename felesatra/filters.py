"""Jinja filters."""

import urllib.parse

filters = {}


def _filter(func):
    """Decorator to register filter."""
    filters[func.__name__] = func
    return func


@_filter
def urljoin(url, base):
    """urljoin filter"""
    return urllib.parse.urljoin(base, url)
