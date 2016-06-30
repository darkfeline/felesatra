import io
import unittest
import xml.etree.ElementTree as ET

from frelia import enja


class SimpleDocumentTestCase(unittest.TestCase):

    def setUp(self):
        builder = ET.TreeBuilder()
        builder.start('content', {})
        builder.start('p', {})
        builder.data('Hello world!')
        builder.end('p')
        builder.end('content')
        content = builder.close()

        self.doc = enja.EnjaDocument(
            metadata={'title': 'Example'},
            content=content)

    def test_inner_content(self):
        """Test getting an enja document's inner_content property."""
        self.assertEqual(self.doc.inner_content, '<p>Hello world!</p>')


class DocumentTextTestCase(unittest.TestCase):

    def setUp(self):
        self.content_string = """<section><h1>Test section</h1>
        <p>Hello world!</p>
        </section>
        """
        self.text = """<enja-document>
        <metadata>{"foo": "bar"}</metadata>
        <content>%s</content>
        </enja-document>
        """ % (self.content_string,)

    def test_simple_document_from_string(self):
        """Test parsing a simple enja document from a string."""
        doc = enja.EnjaDocument.from_string(self.text)
        self.assertEqual(doc.metadata, {'foo': 'bar'})
        self.assertEqual(doc.inner_content, self.content_string)
