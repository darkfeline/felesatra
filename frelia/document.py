"""Document classes.

This module contains Document classes.  These are encapsulations of EnjaDocument
that apply some kind of processing to the document content or metadata.

"""

import abc

from frelia import enja


class Document:

    """Represents a loaded document."""

    def __init__(self, loader, metadata, content):
        self.loader = loader
        self.metadata = metadata
        self.content = content

    def __repr__(self):
        return '{cls}({loader!r}, {metadata!r}, {content!r})'.format(
            cls=type(self).__name__,
            loader=self.loader,
            metadata=self.metadata,
            content=self.content)

    def __eq__(self, other):
        if isinstance(other, type(self)):
            return (
                self.metadata == other.metadata
                and self.content == other.content
            )
        else:
            return NotImplemented


class DocumentLoader(abc.ABC):

    """Abstract document loader base class.

    This defines the interface for document loader classes.

    """

    def __repr__(self):
        return '<{}>'.format(type(self).__name__)

    def load(self, file):
        """Load document from file."""
        document = self._load(file)
        self._transform(document)
        return document

    @abc.abstractmethod
    def _load(self, file):
        """Return metadata and content loaded from file."""

    def _transform(self, document):
        """Transform the document."""


class EnjaDocumentLoader(DocumentLoader):

    def _load(self, file):
        enja_doc = enja.EnjaDocument.load(file)
        return Document(self, enja_doc.metadata, enja_doc.content)


class JinjaDocumentLoader(DocumentLoader):

    def __init__(self, env):
        self.env = env

    def _transform(self, document):
        super()._transform(document)
        content_as_template = self.env.from_string(document.content)
        rendered_content = content_as_template.render()
        document.content = rendered_content
