"""Test utilities."""

import tempfile
import unittest


class FilesTestCase(unittest.TestCase):

    """Fixtures for working with files.

    Provides a safe temporary directory to work with.

    """

    def setUp(self):
        self._tempdir = tempfile.TemporaryDirectory()
        self.workdir = self._tempdir.name

    def tearDown(self):
        self._tempdir.cleanup()

    def assertFileEqual(self, first, second):
        """Assert two files have the same contents."""
        with open(first, 'rb') as firstfile, open(second, 'rb') as secondfile:
            self.assertEqual(firstfile.read(), secondfile.read())
