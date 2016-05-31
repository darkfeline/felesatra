"""Website resource."""

import logging
import os

from felesatra.resources.base import DirectoryResource

from .abc import SiteResource
from .atom import AtomResource
from .blog import Blogpage
from .page import Homepage, HTMLResource
from .sitemap import SitemapResource

logger = logging.getLogger(__name__)


class SiteDirectoryResource(DirectoryResource, SiteResource):

    """DirectoryResource with custom resource loading."""

    def index(self, env):
        # pylint: disable=unused-variable
        for path, resource in self:
            if isinstance(resource, SiteResource):
                resource.index(env)

    @classmethod
    def resource_classes(cls):
        yield Blogpage
        yield SiteDirectoryResource
        yield from super().resource_classes()


class Website(SiteDirectoryResource):

    """Website for rendering.

    Supports many website things.

    - 404 page
    - Home page
    - Sitemap
    - Atom feed
    - Page indexing service

    """

    def __init__(self, path):
        super().__init__(path)

    def index(self, env):
        env.globals['page_index'] = []
        super().index(env)

    @classmethod
    def load(cls, path):
        """Load resource."""
        filename = os.path.basename(path)
        if filename == '404.html':
            return HTMLResource(path)
        if filename == 'index.html':
            return Homepage(path)
        else:
            return super().load(path)

    def render(self, env, target):
        super().render(env, target)

        sitemap = SitemapResource()
        sitemap.render(env, os.path.join(target, 'sitemap.xml'))

        if 'atom_context' in env.globals:
            atom_feed = AtomResource(env.globals['atom_context'])
            atom_feed.render(env, os.path.join(target, 'atom.xml'))
