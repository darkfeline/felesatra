import itertools

import pytest

from frelia import descriptors


def test_property_caching():
    obj = _IncrementValueClass()
    assert obj.value == 0
    assert obj.value == 0
    del obj.value
    assert obj.value == 1
    assert obj.value == 1


def test_explicit_attr():
    obj = _AttrClass()
    assert obj.attr == 1
    assert obj.attr == 1
    del obj.attr
    assert obj.attr == 1
    assert obj.attr == 1


def test_property_access_via_class():
    assert isinstance(
        _IncrementValueClass.value,
        descriptors.CachedProperty)


def test_setting_property():
    obj = _IncrementValueClass()
    obj.value = 1
    assert obj.value == 1


class _AttrClass:
    """Class for testing CachedProperty."""
    attr = descriptors.CachedProperty(lambda self: 1, attr='attr')


class _IncrementValueClass:

    """Class for testing CachedProperty."""

    def __init__(self):
        self.value_iter = itertools.count(0)

    @descriptors.CachedProperty
    def value(self):
        return next(self.value_iter)
