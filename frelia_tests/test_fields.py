import unittest

from frelia import fields


def _class_with_field(field):
    """Make a class with the given field."""
    return type('test', (), {'field': field})


def _obj_with_field(field):
    return _class_with_field(field)()


class BaseFieldTestCase(unittest.TestCase):

    def test_get_class(self):
        test_class = _class_with_field(fields.BaseField())
        self.assertIsInstance(test_class.field, fields.BaseField)

    def test_get_unset_value(self):
        test_obj = _obj_with_field(fields.BaseField())
        with self.assertRaises(AttributeError):
            getattr(test_obj, 'field')

    def test_set_and_get_value(self):
        test_obj = _obj_with_field(fields.BaseField())
        test_obj.field = 1
        self.assertEqual(test_obj.field, 1)

    def test_del_value(self):
        test_obj = _obj_with_field(fields.BaseField())
        with self.assertRaises(AttributeError):
            getattr(test_obj, 'field')

        test_obj.field = 1
        self.assertEqual(test_obj.field, 1)

        del test_obj.field
        with self.assertRaises(AttributeError):
            getattr(test_obj, 'field')


class LazyFieldTestCase(unittest.TestCase):

    def test_get_lazy_field(self):
        """Test getting LazyField default value."""
        test_obj = _obj_with_field(fields.LazyField(1))
        self.assertEqual(test_obj.field, 1)

    def test_set_lazy_field(self):
        """Test that set LazyField value replaces default."""
        test_obj = _obj_with_field(fields.LazyField(1))
        test_obj.field = 2
        self.assertEqual(test_obj.field, 2)


class DefaultFieldTestCase(unittest.TestCase):

    def test_get_default_field(self):
        """Test LazyField default value is created each time."""
        test_class = _class_with_field(fields.DefaultField(list))
        test_obj1 = test_class()
        test_obj2 = test_class()
        self.assertIsNot(test_obj1.field, test_obj2.field)


class TypedFieldTestCase(unittest.TestCase):

    pass
