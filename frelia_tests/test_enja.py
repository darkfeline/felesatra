import textwrap
import unittest

from frelia import enja


class EnjaTestCase(unittest.TestCase):

    def test_fromstring_metadata(self):
        doc = enja.EnjaDocument.from_string("""
        <head>
        <meta name="foo" content="bar">
        <meta name="spam" content="eggs">
        </head>
        """)
        self.assertEqual(doc.metadata, {
            'foo': 'bar',
            'spam': 'eggs',
        })

    def test_fromstring_content(self):
        doc = enja.EnjaDocument.from_string("""
        <body>
        <section>
        <h1>Title</h1>
        <p>text</p>
        <p>more text</p>
        </section>
        </body>
        """)
        self.assertEqual(doc.content, textwrap.dedent("""
        <section>
        <h1>Title</h1>
        <p>text</p>
        <p>more text</p>
        </section>
        """))
