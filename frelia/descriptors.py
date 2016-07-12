class CachedProperty:

    """Cached property descriptor.

    >>> class Foo:
    ...     @CachedProperty
    ...     def foo(self):
    ...         return 1
    ...
    >>> foo = Foo()
    >>> foo.foo
    1

    """

    def __init__(self, fget, attr=None):
        self.fget = fget
        if attr is None:
            self.attr = self.fget.__name__
        else:
            self.attr = attr

    def __repr__(self):
        return '{cls}({fget!r}, attr={attr!r})'.format(
            cls=type(self).__name__,
            fget=self.fget,
            attr=self.attr)

    def __get__(self, instance, owner):
        if instance is None:
            return self
        else:
            value = self.fget(instance)
            setattr(instance, self.attr, value)
            return value
