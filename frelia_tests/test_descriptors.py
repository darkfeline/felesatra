import unittest

from frelia import descriptors


class CachedPropertyTestCase(unittest.TestCase):

    def test_caching(self):
        obj = _IncrementValueClass()
        self.assertEqual(obj.value, 1)
        self.assertEqual(obj.value, 1)
        del obj.value
        self.assertEqual(obj.value, 2)
        self.assertEqual(obj.value, 2)

    def test_class_access(self):
        self.assertIsInstance(
            _IncrementValueClass.value,
            descriptors.CachedProperty)

    def test_set(self):
        obj = _IncrementValueClass()
        with self.assertRaises(AttributeError):
            obj.value = 1

    def test_repr(self):
        self.assertEqual(
            repr(descriptors.CachedProperty(_MockCallable())),
            'CachedProperty(<_MockCallable>)')


class _IncrementValueClass:

    """Class for testing CachedProperty."""

    # pylint: disable=too-few-public-methods

    def __init__(self):
        self.last_value = 0

    @descriptors.CachedProperty
    def value(self):
        self.last_value += 1
        return self.last_value


class _MockCallable:

    """Mock callable with controlled __repr__()."""

    # pylint: disable=too-few-public-methods

    def __call__(self): pass

    def __repr__(self):
        return '<_MockCallable>'
