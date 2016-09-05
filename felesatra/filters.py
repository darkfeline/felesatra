"""Custom Jinja filters."""

import html
import itertools

__all__ = [
    'tagattrs',
    'first',
]


def tagattrs(obj, *attrs):
    """Conditionally make tag attributes from object.

    Return a string of the attributes and values formatted as an HTML tag
    attribute if the object has the attribute, for each given attribute.

    """
    return ' '.join(
        _tagattr(obj, attr)
        for attr in attrs
        if getattr(obj, attr, None) is not None)


def _tagattr(obj, attr):
    """Format an HTML tag attribute string from an object."""
    return '{}="{}"'.format(
        attr,
        html.escape(getattr(obj, attr)))


def first(obj, n):
    """Get the first n items."""
    yield from itertools.islice(obj, n)
