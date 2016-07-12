"""Instance field descriptors."""

import abc
import datetime
import weakref


class Field(abc.ABC):

    """Abstract base class for fields."""

    @abc.abstractmethod
    def __get__(self, instance, owner):
        raise NotImplementedError

    @abc.abstractmethod
    def __set__(self, instance, value):
        raise NotImplementedError

    @abc.abstractmethod
    def __delete__(self, instance):
        raise NotImplementedError


class BaseField(Field):

    """Base field implementation.

    The base field implementation functions transparently, as if the descriptor
    didn't exist and were simply an instance attribute.

    """

    def __init__(self):
        self._values = weakref.WeakKeyDictionary()

    def __get__(self, instance, owner):
        if instance is None:
            return self
        else:
            return self._get_for_instance(instance)

    def _get_for_instance(self, instance):
        try:
            return self._values[instance]
        except KeyError:
            raise AttributeError('Field is not set.')

    def __set__(self, instance, value):
        self._values[instance] = value

    def __delete__(self, instance):
        del self._values[instance]


class LazyField(BaseField):

    """Descriptor for a generic single value field.

    Unlike DefaultField, the default value will not be set on the instance if
    an instance value doesn't exist.

    """

    def __init__(self, default):
        super().__init__()
        self.default = default

    def __get__(self, instance, owner):
        try:
            return super().__get__(instance, owner)
        except AttributeError:
            return self.default


class DefaultField(BaseField):

    """Descriptor for a generic single value field.

    Unlike LazyField, accessing the attribute on an instance will set the
    default value on the instance, like a defaultdict.  This should be used for
    mutable default values.

    """

    def __init__(self, default_func):
        super().__init__()
        self.default_func = default_func

    def __get__(self, instance, owner):
        try:
            return super().__get__(instance, owner)
        except AttributeError:
            default_value = self.default_func()
            self.__set__(instance, default_value)
            return default_value


class DateTimeField(BaseField):

    """Field for datetimes."""

    def __set__(self, instance, value):
        value = self._ensure_datetime(value)
        assert isinstance(value, datetime.datetime)
        super().__set__(instance, value)

    @staticmethod
    def _ensure_datetime(value):
        """Ensure value is datetime (if it is a date instead)."""
        if isinstance(value, datetime.datetime):
            return value
        elif isinstance(value, datetime.date):
            return datetime.datetime(value.year, value.month, value.day)
        else:  # pragma: no cover
            assert False, '{!r} is not date-like'.format(value)
