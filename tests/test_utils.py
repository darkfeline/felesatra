"""Tests for felesatra.utils."""

import unittest
from unittest.mock import Mock

from felesatra.utils import geturl

class GeturlTestCase(unittest.TestCase):

    """Tests for geturl with environment fixture."""

    def setUp(self):
        self.env = Mock(globals={
            'site': {
                'url': 'http://www.example.com',
                'builddir': 'build',
            }
        })

    def test_geturl_file(self):
        """Test geturl on file path."""
        path = 'build/foo.html'
        url = geturl(self.env, path)
        self.assertEqual(url, 'http://www.example.com/foo.html')

    def test_geturl_dir(self):
        """Test geturl on directory path."""
        path = 'build/foo/bar/'
        url = geturl(self.env, path)
        self.assertEqual(url, 'http://www.example.com/foo/bar/')

    def test_geturl_empty(self):
        """Test geturl on empty path."""
        path = 'build/'
        url = geturl(self.env, path)
        self.assertEqual(url, 'http://www.example.com/')
