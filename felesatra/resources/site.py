"""Website resource."""

import logging
import os

from .page import DirectoryResource, Homepage
from .sitemap import SitemapResource

logger = logging.getLogger(__name__)


class Website(DirectoryResource):

    """Website for rendering."""

    def __init__(self, path):
        super().__init__(path)

    @classmethod
    def load(cls, path):
        """Load resource."""
        if os.path.basename(path) == 'index.html':
            return Homepage(path)
        else:
            return super().load(path)

    def render(self, env, target):
        super().render(env, target)
        sitemap = SitemapResource()
        sitemap.render(env, os.path.join(target, 'sitemap.xml'))
