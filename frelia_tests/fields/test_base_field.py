import pytest

from frelia import fields


def test_get_class(field_utils):
    test_class = field_utils.class_with_field(fields.BaseField())
    assert isinstance(test_class.field, fields.BaseField)


def test_get_unset_value(field_utils):
    test_obj = field_utils.obj_with_field(fields.BaseField())
    with pytest.raises(AttributeError):
        getattr(test_obj, 'field')


def test_set_and_get_value(field_utils):
    test_obj = field_utils.obj_with_field(fields.BaseField())
    test_obj.field = 1
    assert test_obj.field == 1


def test_del_value(field_utils):
    test_obj = field_utils.obj_with_field(fields.BaseField())
    with pytest.raises(AttributeError):
        getattr(test_obj, 'field')

    test_obj.field = 1
    assert test_obj.field == 1

    del test_obj.field
    with pytest.raises(AttributeError):
        getattr(test_obj, 'field')
