import pytest

from frelia import fields


class _FieldClass:
    field = fields.BaseField()


@pytest.fixture
def fieldclass():
    return _FieldClass


def test_get_class(fieldclass):
    assert isinstance(fieldclass.field, fields.BaseField)


def test_get_unset_value(fieldclass):
    test_obj = fieldclass()
    with pytest.raises(AttributeError):
        getattr(test_obj, 'field')


def test_set_and_get_value(fieldclass):
    test_obj = fieldclass()
    test_obj.field = 1
    assert test_obj.field == 1


def test_del_value(fieldclass):
    test_obj = fieldclass()
    with pytest.raises(AttributeError):
        getattr(test_obj, 'field')

    test_obj.field = 1
    assert test_obj.field == 1

    del test_obj.field
    with pytest.raises(AttributeError):
        getattr(test_obj, 'field')
