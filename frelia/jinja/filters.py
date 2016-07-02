"""Custom Jinja filters.

This module contains an attribute :data:`filters` which is a dict that can be
used in a Jinja Environment.

"""

import html
import urllib.parse

# A dictionary of filters that can be registered in a Jinja Environment.
filters = {}


def _filter(func):
    """Decorator to register a filter function."""
    filters[func.__name__] = func
    return func


@_filter
def urljoin(url, base):
    """urljoin filter"""
    return urllib.parse.urljoin(base, url)


@_filter
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


@_filter
def first(obj, n):
    """Get the first n items."""
    i = 0
    obj = iter(obj)
    while i < n:
        yield next(obj)
        i += 1
