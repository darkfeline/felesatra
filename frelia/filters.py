"""Custom Jinja filters."""

import html
import urllib.parse

__all__ = [
    'urljoin',
    'tagattrs',
    'first',
]


def urljoin(url, base):
    """urljoin filter"""
    return urllib.parse.urljoin(base, url)


def tagattrs(obj, *attrs):
    """Conditionally make tag attributes from object.

    Return a string of the attributes and values formatted as an HTML tag
    attribute if the object has the attribute, for each given attribute.

    """
    return ' '.join(_tagattr(obj, attr) for attr in attrs)


def _tagattr(obj, attr):
    """Format an HTML tag attribute string from an object."""
    return '{}="{}"'.format(
        attr,
        html.escape(getattr(obj, attr)))


def first(obj, n):
    """Get the first n items."""
    i = 0
    obj = iter(obj)
    while i < n:
        yield next(obj)
        i += 1
