import io
import textwrap

from frelia import enja


def test_load():
    """Test parsing a simple enja document from a file."""
    text = textwrap.dedent("""\
    foo: bar
    ---
    <p>Hello world!</p>""")
    file = io.StringIO(text)
    doc = enja.EnjaDocument.load(file)
    assert doc.metadata == {'foo': 'bar'}
    assert doc.content == '<p>Hello world!</p>'


def test_dump():
    """Test parsing a simple enja document from a file."""
    file = io.StringIO()
    doc = enja.EnjaDocument({'foo': 'bar'}, '<p>Hello world!</p>')
    doc.dump(file)
    assert file.getvalue() == 'foo: bar\n---\n<p>Hello world!</p>'
