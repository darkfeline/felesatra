"""Enja document format.

Enja is a file format for storing documents in files.  This module implements
reading and writing the Enja file format.

An Enja file is a text file that contains:

- the document header metadata formatted in YAML
- a line containing three (3) hyphen-minus characters (U+002D) terminated by a
  newline character (U+000A).
- the document body
"""

import datetime
import io
import pathlib

import yaml

_DIVIDER = '---\n'
_MIN = datetime.date.min


class Document:

    __slots__ = ('header', 'body')

    def __init__(self, body: str):
        self.header = {}
        self.body = body

    def __repr__(self):
        cls = type(self).__qualname__
        return f'<{cls} header={self.header!r} body={self.body!r}>'


def dump(document, file):
    """Write an Enja document."""
    yaml.dump(
        document.header, file,
        Dumper=yaml.CDumper,
        default_flow_style=False)
    file.write(_DIVIDER)
    file.write(document.body)


def load(file):
    """Load an Enja document."""
    header_stream, file = _create_header_stream(file)
    header = _load_header(header_stream)
    body = file.read()
    document = Document(body)
    document.header = header
    return document


def context(document, src: str):
    context = document.header.copy()
    context['content'] = document.body
    if 'published' not in context:
        context['published'] = _parse_published(src)
    if 'modified' not in context:
        context['modified'] = context['published']
    return context


def _parse_published(path: str):
    path = pathlib.PurePath(path)
    try:
        y, m, d = path.parts[-4:-1]
    except ValueError:
        return _MIN
    try:
        return datetime.date(int(y), int(m), int(d))
    except ValueError:
        return _MIN


def _create_header_stream(file):
    """Create metadata stream from a file object.

    Read off the header section from a file object and return that stream
    along with the file object, whose position will be at the start of the
    document body.
    """
    assert isinstance(file, io.TextIOBase)
    header_stream = io.StringIO()
    for line in file:
        if line == _DIVIDER:
            break
        header_stream.write(line)
    header_stream.seek(0)
    return header_stream, file


def _load_header(stream):
    header = yaml.load(stream, Loader=yaml.CLoader)
    if header is None:
        return {}
    assert isinstance(header, dict)
    return header
