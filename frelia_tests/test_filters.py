import itertools
import string
from unittest import mock
import urllib.parse

import hypothesis
from hypothesis import strategies

from frelia import filters

_URL_ALPHABET = string.ascii_lowercase + '/'
_url_text = strategies.text(
    alphabet=_URL_ALPHABET,
    max_size=8)


@hypothesis.given(_url_text, _url_text)
@hypothesis.settings(max_examples=50)
def test_urljoin(base, url):
    assert filters.urljoin(url, base) == urllib.parse.urljoin(base, url)


def test_tagattrs():
    obj = mock.NonCallableMock(
        [],
        foo='bar',
        spam='eggs"',
        cloche='pastalie')
    got = filters.tagattrs(obj, 'spam', 'foo')
    assert got == 'spam="eggs&quot;" foo="bar"'


def test_first():
    got = list(filters.first(itertools.count(1), 4))
    assert got == [1, 2, 3, 4]
