import io
import textwrap

import pytest

from frelia import enja


@pytest.fixture
def simple_doc():
    text = textwrap.dedent("""\
    foo: bar
    ---
    <p>Hello world!</p>""")
    return io.StringIO(text)


def test_parse_simple_document(simple_doc):
    """Test parsing a simple enja document from a file."""
    doc = enja.EnjaDocument.parse(simple_doc)
    assert doc.metadata == {'foo': 'bar'}
    assert doc.content == '<p>Hello world!</p>'
