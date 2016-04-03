"""Tests for felesatra.filters"""

import unittest

from felesatra import filters


class FilterTestMeta(type):

    """Metaclass for writing filter tests."""

    def __new__(mcs, name, parents, dct):
        test_func = dct.pop('Meta').test_func
        for attr, test_meta in list(dct.items()):
            # Replace test meta objects with generated test method.
            if attr.startswith('test_') and isinstance(test_meta, type):
                test = mcs.make_test(test_func, test_meta)
                dct[attr] = test
        parents += (unittest.TestCase,)
        return super().__new__(mcs, name, parents, dct)

    @staticmethod
    def make_test(test_func, test_meta):
        """Make test from meta object."""
        def test(self):
            """Filter test."""
            self.assertEqual(
                test_func(*test_meta.args, **test_meta.kwargs),
                test_meta.expected,
            )
        test.__doc__ = test_meta.__doc__
        return test


class URLTestCase(metaclass=FilterTestMeta):

    """URL tests."""

    # pylint: disable=too-few-public-methods
    # pylint: disable=missing-docstring

    class Meta:
        test_func = filters.url

    class test_url:
        """Test url."""
        args = ['foo', 'http://www.example.com']
        kwargs = {}
        expected = 'http://www.example.com/foo'

    class test_url_base_trailing:
        """Test url with trailing slash on base."""
        args = ['foo', 'http://www.example.com/']
        kwargs = {}
        expected = 'http://www.example.com/foo'

    class test_url_base_trailing_many:
        """Test url with many trailing slashes on base."""
        args = ['foo', 'http://www.example.com/////']
        kwargs = {}
        expected = 'http://www.example.com/foo'

    class test_url_base_path:
        """Test url with path on base."""
        args = ['foo', 'http://www.example.com/foo/']
        kwargs = {}
        expected = 'http://www.example.com/foo/foo'

    class test_url_ref_leading:
        """Test url with leading slash on ref."""
        args = ['/foo', 'http://www.example.com/bar']
        kwargs = {}
        expected = 'http://www.example.com/foo'

    class test_url_ref_leading_two:
        """Test url with two leading slashes on ref."""
        args = ['//foo', 'http://www.example.com/bar']
        kwargs = {}
        expected = 'http://foo'

    class test_url_ref_leading_many:
        """Test url with many leading slashes on ref."""
        args = ['/////foo', 'http://www.example.com/bar']
        kwargs = {}
        expected = 'http://www.example.com///foo'

    class test_url_ref_trailing:
        """Test url with trailing slash on ref."""
        args = ['foo/', 'http://www.example.com']
        kwargs = {}
        expected = 'http://www.example.com/foo/'

    class test_url_ref_trailing_many:
        """Test url with trailing slashes on ref."""
        args = ['foo/////', 'http://www.example.com']
        kwargs = {}
        expected = 'http://www.example.com/foo/'
