import itertools
from unittest import mock

from frelia import filters


def test_tagattrs():
    obj = mock.NonCallableMock(
        [],
        foo='bar',
        spam='eggs"',
        cloche='pastalie',
        none=None)
    got = filters.tagattrs(obj, 'spam', 'foo', 'none')
    assert got == 'spam="eggs&quot;" foo="bar"'


def test_first():
    got = list(filters.first(itertools.count(1), 4))
    assert got == [1, 2, 3, 4]
