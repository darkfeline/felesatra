import unittest

import frelia.descriptors


class _IncrementValueClass:

    def __init__(self):
        self.last_value = 0

    @frelia.descriptors.CachedProperty
    def value(self):
        self.last_value += 1
        return self.last_value


class CachedPropertyTestCase(unittest.TestCase):

    def test_caching(self):
        obj = _IncrementValueClass()
        self.assertEqual(obj.value, 1)
        self.assertEqual(obj.value, 1)
        del obj.value
        self.assertEqual(obj.value, 2)
        self.assertEqual(obj.value, 2)
