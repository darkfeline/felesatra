"""URLs and linking."""

from urllib.parse import urljoin


def url(ref, base):
    """Make canonical URL for given reference.

    base is the protocol and network location parts of the URL.

    """
    return urljoin(base, ref)
