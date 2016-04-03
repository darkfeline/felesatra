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


def tagattr(obj, attr):
    """Conditionally make tag attribute from object."""
    if hasattr(obj, attr):
        return '{}="{}"'.format(attr, getattr(obj, attr))


@_filter
def tagattrs(obj, *attrs):
    """Conditionally make tag attributes from object."""
    return ' '.join(tagattr(obj, attr) for attr in attrs)
