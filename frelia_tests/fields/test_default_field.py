from frelia import fields


def test_get_default_field(field_utils):
    """Test LazyField default value is created each time."""
    test_class = field_utils.class_with_field(fields.DefaultField(list))
    test_obj1 = test_class()
    test_obj2 = test_class()
    assert test_obj1.field is not test_obj2.field
