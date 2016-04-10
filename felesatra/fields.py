"""Instance field descriptors.

This module contains instance field descriptor classes that are handy for rendering.

"""

import datetime
from weakref import WeakKeyDictionary


class Field:

    """Descriptor for a generic single value field."""

    # pylint: disable=too-few-public-methods

    def __init__(self, default=None):
        self.default = default
        self.values = WeakKeyDictionary()

    def __get__(self, obj, objtype):
        if obj is None:
            raise AttributeError('Cannot access field from class.')
        return self.values.get(obj, self.default)

    def __set__(self, obj, value):
        self.values[obj] = value


class DateTimeField(Field):

    """Field for datetimes."""

    # pylint: disable=too-few-public-methods

    def __set__(self, obj, value):
        if isinstance(value, datetime.date):
            value = datetime.datetime(value.year, value.month, value.day)
        if not isinstance(value, datetime.datetime):
            raise TypeError('value must be datetime')
        super().__set__(obj, value)
