"""Website resource."""

import logging
import os
from collections import OrderedDict
from urllib.parse import urljoin

from . import base
from .base import DirectoryResource
from .atom import AtomResource, Author, Link
from .blog import Blogpage
from .page import Homepage, HTMLResource
from .sitemap import SitemapResource

logger = logging.getLogger(__name__)


class CustomDirectoryResource(DirectoryResource):

    """DirectoryResource with custom resource loading."""

    @classmethod
    def resource_classes(cls):
        yield Blogpage
        yield DirectoryResource
        for resource_class in super().resource_classes():
            yield resource_class


class Website(CustomDirectoryResource):

    """Website for rendering."""

    def __init__(self, path, site_url):
        super().__init__(path)
        self.site_url = urljoin(site_url, '/')

    def walk(self, env):
        env.globals['site'] = {
            'url': self.site_url,
            'srcdir': self.path,
            'nav': OrderedDict((
                ('About', 'about/'),
                ('Projects', 'projects/'),
            )),
        }
        env.globals['sitemap'] = []
        env.globals['atom_entries'] = []
        env.globals['pages'] = []
        super().walk(env)
        env.globals['sitemap'] = sorted(env.globals['sitemap'])
        env.globals['atom_entries'] = sorted(env.globals['atom_entries'])
        env.globals['pages'] = sorted(env.globals['pages'])

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
        logger.debug('Pages %r', env.globals['pages'])
        super().render(env, target)

        sitemap = SitemapResource()
        sitemap.render(env, os.path.join(target, 'sitemap.xml'))

        atom_feed = AtomResource({
            'id': self.site_url,
            'title': 'Feles Atra',
            'links': [Link(
                urljoin(self.site_url, 'atom.xml'),
                'self',
                'application/atom+xml')],
            'authors': [Author('Allen Li', self.site_url, None)],
            'rights': 'Copyright 2011-2016 Allen Li',  # XXX
        })
        atom_feed.render(env, os.path.join(target, 'atom.xml'))
