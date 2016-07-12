"""Document loading and processing.

This module contains Document classes.  Documents represent content with
metadata.  This class primarily provides an abstraction for Documents and a
DocumentLoader framework that allows Document metadata and content to be
transformed on load.

"""

import abc

from frelia import enja


class Document:

    """Represents a loaded document."""

    def __init__(self, metadata, content, *, loader=None):
        self.loader = loader
        self.metadata = metadata
        self.content = content

    def __repr__(self):
        return '{cls}({metadata!r}, {content!r}, loader={loader!r})'.format(
            cls=type(self).__name__,
            loader=self.loader,
            metadata=self.metadata,
            content=self.content)


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


class RawDocumentLoader(DocumentLoader):

    """DocumentLoader that delegates loading to a raw document class.

    Raw document classes implement the load() classmethod, which take a file
    object and return an instance of the class, representing the loaded
    document.

    """

    def __init__(self, raw_document_class):
        self.raw_document_class = raw_document_class

    def _load(self, file):
        enja_doc = self.raw_document_class.load(file)
        return Document(enja_doc.metadata, enja_doc.content, loader=self)


class JinjaDocumentLoader(DocumentLoader):

    def __init__(self, env, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.env = env

    def _transform(self, document):
        super()._transform(document)
        content_as_template = self.env.from_string(document.content)
        rendered_content = content_as_template.render()
        document.content = rendered_content
