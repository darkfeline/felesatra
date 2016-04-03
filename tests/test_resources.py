"""Tests for felesatra.resources"""

import os

from jinja2 import Environment

from felesatra.resources.base import SimpleFileResource, DirectoryResource

from .utils import FilesTestCase


class EnvTestCase(FilesTestCase):

    """Fixture that includes a rendering environment"""

    def setUp(self):
        super().setUp()
        self.env = Environment()


class FileTestCase(EnvTestCase):

    """Fixture with a single file."""

    def setUp(self):
        super().setUp()
        self.filepath = self.makefile('test data', 'test')

    def test_simple_file_render(self):
        """Render a simple file at top level."""
        resource = SimpleFileResource(self.filepath)
        target = self.getpath('test2')
        resource.render(self.env, target)
        self.assertFileEqual(self.filepath, target)


class SiteTestCase(EnvTestCase):

    """Fixture with a test site."""

    def setUp(self):
        super().setUp()
        self.site = self.makedirs('site')
        self.makefile('content 1', 'site', 'file1')
        dir1 = self.makedirs('site', 'dir1')
        self.makefile('content 2', dir1, 'file2')
        self.makefile('content 3', dir1, 'file3')

    def test_directory_render(self):
        """Render a simple site."""
        resource = DirectoryResource(self.site)
        target = self.getpath('build')
        resource.render(self.env, target)
        self.assertDirEqual(self.site, target)
