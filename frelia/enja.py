import io

import yaml


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
        metadata_stream, file = cls._create_metadata_stream(file)
        metadata = yaml.load(metadata_stream, Loader=yaml.CLoader)
        content = file.read()
        return cls(metadata, content)

    @classmethod
    def _create_metadata_stream(cls, file):
        """Create metadata stream from a file object.

        Read off the metadata section from a file object and return that stream
        along with the file object, whose stream position will be at the start
        of the document content.

        """
        metadata_stream = io.StringIO()
        for line in file:
            if line == '---\n':
                break
            else:
                metadata_stream.write(line)
        metadata_stream.seek(0)
        return metadata_stream, file
