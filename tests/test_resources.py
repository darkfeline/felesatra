"""Tests for felesatra.resources"""

import os

from jinja2 import Environment

from felesatra.resources.base import SimpleFileResource

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
        self.filepath = os.path.join(self.workdir, 'test')
        with open(self.filepath, 'w') as file:
            file.write('test data')

    def test_simple_file_render(self):
        """Render a simple file at top level."""
        resource = SimpleFileResource(self.filepath)
        target = os.path.join(self.workdir, 'test2')
        resource.render(self.env, target)
        self.assertFileEqual(self.filepath, target)
