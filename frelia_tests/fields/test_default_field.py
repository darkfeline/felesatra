import pytest

from frelia import fields


class _DefaultClass:
    field = fields.DefaultField(list)


@pytest.fixture
def defaultclass():
    return _DefaultClass


def test_get_default_field(defaultclass):
    """Test LazyField default value is created each time."""
    test_obj1 = defaultclass()
    test_obj2 = defaultclass()
    assert test_obj1.field is not test_obj2.field
