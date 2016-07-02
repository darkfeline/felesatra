import io
import textwrap
import unittest

from frelia import enja


class SimpleDocumentTestCase(unittest.TestCase):

    def setUp(self):
        self.doc = enja.EnjaDocument(
            metadata={'title': 'Example'},
            content='<p>Hello world!</p>')

    def test_repr(self):
        self.assertEqual(
            repr(self.doc),
            "EnjaDocument({'title': 'Example'}, '<p>Hello world!</p>')")


class DocumentTextTestCase(unittest.TestCase):

    def setUp(self):
        text = textwrap.dedent("""\
        foo: bar
        ---
        <p>Hello world!</p>""")
        self.file = io.StringIO(text)

    def test_parse_simple_document_(self):
        """Test parsing a simple enja document from a file."""
        doc = enja.EnjaDocument.parse(self.file)
        self.assertEqual(doc.metadata, {'foo': 'bar'})
        self.assertEqual(doc.content, '<p>Hello world!</p>')
