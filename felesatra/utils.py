"""Shared utilities."""

import functools
import xml.etree.ElementTree as ET


def tostring(element):
    """Customized version of ET.tostring()."""
    return ET.tostring(element, encoding='unicode', method='html')


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
