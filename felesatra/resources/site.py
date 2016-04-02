"""Website resource."""

import logging
import os
from collections import OrderedDict

from jinja2 import Environment, PackageLoader

from felesatra.utils import cached_property
from felesatra import filters

from .base import DirectoryResource
from .page import HTMLResource, Webpage

logger = logging.getLogger(__name__)


class Website(DirectoryResource):

    """Website for rendering."""

    def __init__(self, path, site_url):
        super().__init__(path)
        self.site_url = site_url

    @cached_property
    def env(self):
        """Get Jinja environment."""
        env = Environment(
            loader=PackageLoader('felesatra', 'templates'),
            trim_blocks=True,
            lstrip_blocks=True,
            auto_reload=False)
        env.filters = filters.filters
        env.globals = {
            'site': {
                'url': self.site_url,
                'nav': OrderedDict((
                    ('Keihan', 'keihan/'),
                )),
            },
        }
        return env

    @classmethod
    def load(cls, path):
        """Load resource."""
        if os.path.basename(path) == 'index.html':
            return HTMLResource(path)
        elif path.endswith('.html'):
            return Webpage(path)
        else:
            return super().load(path)
