"""Jinja filters."""

from urllib.parse import urljoin

filters = {}


def _filter(func):
    """Decorator to register filter."""
    filters[func.__name__] = func
    return func


@_filter
def url(ref, base):
    """Make canonical URL for given reference.

    base is the protocol and network location parts of the URL.

    """
    return urljoin(base, ref)
