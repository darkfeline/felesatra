import itertools
import unittest
import urllib.parse

import hypothesis
from hypothesis import strategies

from frelia.jinja import filters


class FiltersTestCase(unittest.TestCase):

    @hypothesis.given(strategies.text(), strategies.text())
    def test_urljoin(self, base, url):
        self.assertEqual(
            filters.urljoin(url, base),
            urllib.parse.urljoin(base, url))

    def test_tagattrs(self):
        obj = _MockWithFields(
            foo='bar',
            spam='eggs"',
            cloche='pastalie')
        self.assertEqual(
            filters.tagattrs(
                obj, 'spam', 'foo'),
            'spam="eggs&quot;" foo="bar"')

    def test_first(self):
        self.assertEqual(
            list(filters.first(itertools.count(1), 4)),
            [1, 2, 3, 4])


class _MockWithFields:

    def __init__(self, **fields):
        for attr, value in fields.items():
            setattr(self, attr, value)
