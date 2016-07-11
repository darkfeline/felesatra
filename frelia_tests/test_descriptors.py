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


def test_property_access_via_class():
    assert isinstance(
        _IncrementValueClass.value,
        descriptors.CachedProperty)


def test_setting_property():
    obj = _IncrementValueClass()
    with pytest.raises(AttributeError):
        obj.value = 1


class _IncrementValueClass:

    """Class for testing CachedProperty."""

    def __init__(self):
        self.value_iter = itertools.count(0)

    @descriptors.CachedProperty
    def value(self):
        return next(self.value_iter)
