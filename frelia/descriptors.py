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
        self.cache = weakref.WeakKeyDictionary()

    def __get__(self, instance, owner):
        if instance is None:
            return self
        if instance not in self.cache:
            self.cache[instance] = self.fget(instance)
        return self.cache[instance]

    def __set__(self, instance, value):
        raise AttributeError('CachedProperty cannot be set.')

    def __delete__(self, instance):
        self.cache.pop(instance, None)