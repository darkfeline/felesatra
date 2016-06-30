"""Instance field descriptors."""

import abc
import datetime
import weakref


class Field(abc.ABC):

    """Abstract base class for fields."""

    # pylint: disable=too-few-public-methods

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

    # pylint: disable=too-few-public-methods

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


class TypedFieldMeta(type):

    # pylint: disable=too-few-public-methods

    def __new__(cls, name, bases, dct): pass

    def __init__(self, default):
        super().__init__()
        self.default = default


class LazyField(BaseField):

    """Descriptor for a generic single value field.

    Unlike DefaultField, the default value will not be set on the instance if
    an instance value doesn't exist.

    """

    # pylint: disable=too-few-public-methods

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

    # pylint: disable=too-few-public-methods

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


class BoolField(LazyField):

    """Boolean Field."""

    # pylint: disable=too-few-public-methods

    def __init__(self, default=False):
        assert isinstance(default, bool)
        super().__init__(default)


class StringField(LazyField):

    """String Field."""

    # pylint: disable=too-few-public-methods

    def __init__(self, default=''):
        assert isinstance(default, str)
        super().__init__(default)


class ListField(DefaultField):

    """List field."""

    # pylint: disable=too-few-public-methods

    def __init__(self, default_func=list):
        assert isinstance(default_func(), list)
        super().__init__(default_func)


class DateTimeField(LazyField):

    """Field for datetimes."""

    # pylint: disable=too-few-public-methods

    def __init__(self, default):
        assert isinstance(default, datetime.datetime)
        super().__init__(default)

    def __set__(self, instance, value):
        if isinstance(value, datetime.date):
            value = datetime.datetime(value.year, value.month, value.day)
        assert isinstance(value, datetime.datetime)
        super().__set__(instance, value)
