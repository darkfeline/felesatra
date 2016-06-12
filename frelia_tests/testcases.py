"""Extended TestCase classes."""

import os
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

    def getpath(self, path, *paths):
        """Get path relative to working directory."""
        return os.path.join(self.workdir, path, *paths)

    def makefile(self, content, path, *paths):
        """Make a test file with content."""
        filepath = self.getpath(path, *paths)
        with open(filepath, 'w') as file:
            file.write(content)
        return filepath

    def makedirs(self, path, *paths):
        """Make a test directory."""
        dirpath = self.getpath(path, *paths)
        os.makedirs(dirpath, exist_ok=True)
        return dirpath

    def assertFileEqual(self, first, second):
        """Assert two files are equal, including special files."""
        if os.path.isdir(first):
            self.assertDirEqual(first, second)
        elif os.path.isfile(first):
            self.assertRegularFileEqual(first, second)

    def assertRegularFileEqual(self, first, second):
        """Assert two files have the same contents."""
        self.assertTrue(os.path.isfile(first))
        self.assertTrue(os.path.isfile(second))
        with open(first, 'rb') as firstfile, open(second, 'rb') as secondfile:
            self.assertEqual(firstfile.read(), secondfile.read())

    def assertDirEqual(self, first, second):
        """Assert two directories have the same contents."""
        self.assertTrue(os.path.isdir(first))
        self.assertTrue(os.path.isdir(second))
        firstfiles = set(os.listdir(first))
        secondfiles = set(os.listdir(second))
        self.assertEqual(firstfiles, secondfiles)
        for file in firstfiles:
            self.assertFileEqual(
                os.path.join(first, file),
                os.path.join(second, file))
