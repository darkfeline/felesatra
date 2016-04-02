"""Website resource."""

import logging
import os

from jinja2 import Environment, FileSystemLoader

from felesatra.utils import cached_property
from felesatra import filters

from .base import DirectoryResource
from .page import HTMLResource, Webpage

logger = logging.getLogger(__name__)


class Website(DirectoryResource):

    """Website for rendering."""

    _TEMPLATE_DIR = '_templates'

    @cached_property
    def env(self):
        """Get Jinja environment."""
        env = Environment(
            loader=FileSystemLoader(self.getpath(self._TEMPLATE_DIR)),
            trim_blocks=True,
            lstrip_blocks=True,
            auto_reload=False)
        env.filters = filters.filters
        env.globals = {
            'site': {
                'url': 'https://www.felesatra.moe',
            },
        }
        return env

    def __iter__(self):
        for filename, resource in super().__iter__():
            if filename == self._TEMPLATE_DIR:
                continue
            yield filename, resource

    @classmethod
    def load(cls, path):
        """Load resource."""
        if path == 'index.html':
            return HTMLResource(path)
        elif path.endswith('.html'):
            return Webpage(path)
        else:
            return super().load(path)

    def render(self, env, target):
        """Render this resource into target."""
        for path, resource in self:
            logger.debug('Render %s, %s', path, resource)
            resource.render(env, os.path.join(target, path))
