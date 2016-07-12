import pytest

from frelia import fields


class _LazyClass:
    field = fields.LazyField(1)


@pytest.fixture
def lazyclass():
    return _LazyClass


def test_get_lazy_field(lazyclass):
    """Test getting LazyField default value."""
    test_obj = lazyclass()
    assert test_obj.field == 1


def test_set_lazy_field(lazyclass):
    """Test that set LazyField value replaces default."""
    test_obj = lazyclass()
    test_obj.field = 2
    assert test_obj.field == 2
