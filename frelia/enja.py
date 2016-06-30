"""enja is an XML document format.

enja primarily provides a means for providing metadata for an HTML document and
extending HTML with useful markup.

"""

import io
import json
import xml.etree.ElementTree as ET

from frelia import descriptors


class EnjaDocument:

    """Basic enja document class.

    This provides the basic functionality of parsing and representing an enja
    document, and no more.

    """

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content

    def __repr__(self):
        return '<{classname} object with metadata {metadata!r}>'.format(
            classname=type(self).__name__,
            metadata=self.metadata)

    @classmethod
    def parse(cls, file):
        """Parse an enja document from a file object."""
        tree = ET.parse(file)
        root = tree.getroot()
        metadata = json.loads(root.find('metadata').text)
        content_element = root.find('content')
        return cls(metadata, content_element)

    @classmethod
    def from_string(cls, string):
        """Parse an enja document from a string."""
        return cls.parse(io.StringIO(string))

    @descriptors.CachedProperty
    def inner_content(self):
        """Get content as a string."""
        return ''.join(
            ET.tostring(elem, encoding='unicode')
            for elem in self.content)
