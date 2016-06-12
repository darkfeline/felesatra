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

    def __init__(self):
        self.values = weakref.WeakKeyDictionary()

    def __get__(self, instance, owner):
        if instance is None:
            return self
        else:
            return self.values[instance]

    def __set__(self, instance, value):
        self.values[instance] = value

    def __delete__(self, instance):
        del self.values[instance]


class LazyField(BaseField):

    """Descriptor for a generic single value field.

    Unlike DefaultField, the default value will not be set on the instance if
    an instance value doesn't exist.

    """

    def __init__(self, default):
        super().__init__()
        self.default = default

    def __get__(self, instance, owner):
        if instance is None:
            return self
        else:
            return self.values.get(instance, self.default)


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
        except KeyError:
            default_value = self.default_func()
            self.__set__(instance, default_value)
            return default_value


class BoolField(LazyField):

    """Boolean Field."""

    def __init__(self, default=False):
        assert isinstance(default, bool)
        super().__init__(default)


class StringField(LazyField):

    """String Field."""

    def __init__(self, default=''):
        assert isinstance(default, str)
        super().__init__(default)


class AttrField(BaseField):

    """A string field used for rendering HTML tag attributes.

    See the tagattrs() filter.

    """

    def __set__(self, instance, value):
        assert isinstance(value, str)
        super().__set__(instance, value)


class ListField(DefaultField):

    """List field."""

    def __init__(self, default_func=lambda: []):
        assert isinstance(default_func(), list)
        super().__init__(default_func)


class DateTimeField(LazyField):

    """Field for datetimes."""

    def __init__(self, default):
        assert isinstance(default, datetime.datetime)
        super().__init__(default)

    def __set__(self, instance, value):
        if isinstance(value, datetime.date):
            value = datetime.datetime(value.year, value.month, value.day)
        assert isinstance(value, datetime.datetime)
        super().__set__(instance, value)
