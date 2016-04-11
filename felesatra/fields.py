"""Instance field descriptors.

This module contains instance field descriptor classes that are handy for rendering.

"""

import datetime
from abc import ABC
from weakref import WeakKeyDictionary


# pylint: disable=too-few-public-methods


class Field(ABC):

    """Abstract base class for fields."""

    def __init__(self):
        self.values = WeakKeyDictionary()

    def __set__(self, obj, value):
        self.values[obj] = value


class StrictField(Field):

    """Descriptor for a generic single value field.

    This field strictly requires being set and doesn't support defaults.

    """

    def __get__(self, obj, objtype):
        if obj is None:
            raise AttributeError('Cannot access field from class.')
        if obj not in self.values:
            raise AttributeError('Instance field is not set.')
        return self.values[obj]


class LazyField(Field):

    """Descriptor for a generic single value field.

    Unlike DefaultField, the default value will not be set on the instance if
    an instance value doesn't exist.

    """

    def __init__(self, default=None):
        super().__init__()
        self.default = default

    def __get__(self, obj, objtype):
        if obj is None:
            raise AttributeError('Cannot access field from class.')
        return self.values.get(obj, self.default)


class DefaultField(Field):

    """Descriptor for a generic single value field.

    This field is different because accessing it on an instance will set the
    value if it doesn't exist, like a defaultdict.

    """

    def __init__(self, default=lambda: None):
        super().__init__()
        self.default = default

    def __get__(self, obj, objtype):
        if obj is None:
            raise AttributeError('Cannot access field from class.')
        if obj not in self.values:
            self.__set__(obj, self.default())
        return self.values[obj]


class BoolField(LazyField):

    """Boolean Field."""

    def __init__(self, default=False):
        super().__init__(default)


class StringField(LazyField):

    """String Field."""

    def __init__(self, default=''):
        super().__init__(default)


class AttrField(StrictField):

    """A strict string field used for rendering HTML tag attributes.

    See the tagattrs() filter.

    """

    def __set__(self, obj, value):
        if not isinstance(value, str):
            raise TypeError('AttrField value must be string')
        super().__set__(obj, value)


class ListField(DefaultField):

    """List Field.

    This field is different because accessing it on an instance will set the
    value if it doesn't exist, like a defaultdict.

    """

    def __init__(self, default=lambda: []):
        super().__init__(default)


class DateTimeField(LazyField):

    """Field for datetimes."""

    def __set__(self, obj, value):
        if isinstance(value, datetime.date):
            value = datetime.datetime(value.year, value.month, value.day)
        if not isinstance(value, datetime.datetime):
            raise TypeError('value must be datetime')
        super().__set__(obj, value)
