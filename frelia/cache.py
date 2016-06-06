"""Caching related module."""

import weakref


class CachedProperty:

    def __init__(self, method_func):
        self.method_func = method_func
        self.cached_values = weakref.WeakKeyDictionary()

    def __get__(self, instance, owner):
        if instance is None:
            return self
        value = self.method_func(instance)
        self.cached_values[instance] = value
        return value

    def __set__(self, instance, value):
        raise AttributeError('CachedProperty cannot be set.')
