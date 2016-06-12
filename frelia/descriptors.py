import weakref


class CachedProperty:

    """
    >>> class Foo:
    ...     @CachedProperty
    ...     def foo(self):
    ...         return 1
    ...
    >>> foo = Foo()
    >>> foo.foo
    1

    """

    def __init__(self, fget):
        self.fget = fget
        self.cached_values = weakref.WeakKeyDictionary()

    def __get__(self, instance, owner):
        if instance is None:
            return self
        else:
            return self._get_value_for_instance(instance)

    def _get_value_for_instance(self, instance):
        if not self._instance_has_value(instance):
            self._set_instance_value_from_getter(instance)
        return self.cached_values[instance]

    def _set_instance_value_from_getter(self, instance):
        value = self.fget(instance)
        self.cached_values[instance] = value

    def _instance_has_value(self, instance):
        return instance in self.cached_values

    def __set__(self, instance, value):
        raise AttributeError('CachedProperty cannot be set.')

    def __delete__(self, instance):
        self.cached_values.pop(instance, None)
