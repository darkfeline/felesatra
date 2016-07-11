import pytest


class FieldUtils:

    @staticmethod
    def class_with_field(field):
        """Make a class with the given field."""
        return type('test', (), {'field': field})

    @classmethod
    def obj_with_field(cls, field):
        return cls.class_with_field(field)()


@pytest.fixture
def field_utils():
    return FieldUtils
