"""Website class."""

from jinja2 import Environment, FileSystemLoader

from .resources import DirectoryResource
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

    def render(self):
        return ''
