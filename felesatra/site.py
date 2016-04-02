"""Website class."""

import os

from jinja2 import Environment, FileSystemLoader

from .resources import DirectoryResource, RenderError, SimpleFileResource
from .utils import cached_property


class Website(DirectoryResource):

    """Website for rendering."""

    @cached_property
    def env(self):
        """Get Jinja environment."""
        return Environment(
            loader=FileSystemLoader(self.getpath('templates')),
            trim_blocks=True,
            lstrip_blocks=True,
            auto_reload=False)

    def __iter__(self):
        for filename in os.listdir(self.path):
            if os.path.isdir(filename):
                yield filename, DirectoryResource(filename)
            elif os.path.isfile(filename):
                yield filename, SimpleFileResource(filename)
            else:
                raise RenderError('Unknown file %s', filename)
