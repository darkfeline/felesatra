"""Document files with in-band metadata.

The enja module provides an implementation for reading enja documents, which
are text file documents of an unspecified format accompanied with structured
metadata.

An enja document file is a text file that contains YAML metadata separated from
the succeeding document content by a line containing three hyphen-minus
characters (U+002D).

"""

import io

import yaml


class EnjaDocument:

    """This class represents an enja document.

    Instances can be created by directly supplying the metadata and content:

    >>> x = EnjaDocument({'title': 'Example'}, 'hello')
    >>> x.metadata
    {'title': 'Example'}
    >>> x.content
    'hello'

    However, most often you will be parsing enja documents from files:

    >>> x = EnjaDocument.load(io.StringIO('''
    ... foo: bar
    ... ---
    ... hello'''))
    >>> x.metadata
    {'foo': 'bar'}
    >>> x.content
    'hello'

    """

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content

    def __repr__(self):
        return '{cls}({metadata!r}, {content!r})'.format(
            cls=type(self).__name__,
            metadata=self.metadata,
            content=self.content)

    @classmethod
    def load(cls, file):
        """Load an enja document from a file object."""
        metadata_stream, file = cls._create_metadata_stream(file)
        metadata = yaml.load(metadata_stream, Loader=yaml.CLoader)
        content = file.read()
        return cls(metadata, content)

    def dump(self, file):
        """Dump an enja document to a file object."""
        yaml.dump(
            self.metadata,
            file,
            Dumper=yaml.CDumper,
            default_flow_style=False)
        file.write('---\n')
        file.write(self.content)

    @staticmethod
    def _create_metadata_stream(file):
        """Create metadata stream from a file object.

        Read off the metadata section from a file object and return that stream
        along with the file object, whose stream position will be at the start
        of the document content.

        """
        assert isinstance(file, io.TextIOBase)
        metadata_stream = io.StringIO()
        for line in file:
            if line == '---\n':
                break
            else:
                metadata_stream.write(line)
        metadata_stream.seek(0)
        return metadata_stream, file
