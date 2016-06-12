import unittest

import frelia.fields


def _class_with_field(field):
    """Make a class with given field."""
    return type('test', (), {'field': field})


class FieldTestCase(unittest.TestCase):

    def test_get_lazy_field(self):
        """Test getting LazyField default value."""
        test_obj = _class_with_field(frelia.fields.LazyField(1))()
        self.assertEqual(test_obj.field, 1)

    def test_set_lazy_field(self):
        """Test that set LazyField value replaces default."""
        test_obj = _class_with_field(frelia.fields.LazyField(1))()
        test_obj.field = 2
        self.assertEqual(test_obj.field, 2)

    def test_get_default_field(self):
        """Test LazyField default value is created each time."""
        test_class = _class_with_field(frelia.fields.DefaultField(lambda: []))
        test_obj1 = test_class()
        test_obj2 = test_class()
        self.assertIsNot(test_obj1.field, test_obj2.field)
