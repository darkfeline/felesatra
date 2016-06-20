"""Serialization between Python types and strings."""

from collections import namedtuple

Serialization = namedtuple('Serialization', ['serializer', 'deserializer'])


class Serializer:

    def __init__(self):
        self.serializations = []

    def serialize(self, obj):
        pass

    def deserialize(self, s):
        pass
