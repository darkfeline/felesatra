"""Custom Jinja filters."""

import urllib.parse

# A dictionary of filters that can be registered in a Jinja Environment.
filters = {}


def _filter(func):
    """Decorator to register filter."""
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
    return ' '.join(tagattr(obj, attr) for attr in attrs if hasattr(obj, attr))


def tagattr(obj, attr):
    """Make HTML tag attribute from object.

    Return a string of the attribute and value formatted as an HTML tag
    attribute.

    """
    return '{}="{}"'.format(attr, getattr(obj, attr))


@_filter
def first(obj, n):
    """Get the first n items."""
    i = 0
    obj = iter(obj)
    while i < n:
        yield next(obj)
        i += 1
