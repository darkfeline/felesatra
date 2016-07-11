from frelia import fields


def test_get_lazy_field(field_utils):
    """Test getting LazyField default value."""
    test_obj = field_utils.obj_with_field(fields.LazyField(1))
    assert test_obj.field == 1


def test_set_lazy_field(field_utils):
    """Test that set LazyField value replaces default."""
    test_obj = field_utils.obj_with_field(fields.LazyField(1))
    test_obj.field = 2
    assert test_obj.field == 2
