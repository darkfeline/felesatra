"""Website resource."""

import logging
import os
from collections import OrderedDict
from urllib.parse import urljoin

from .atom import AtomResource, Author, Link
from .page import DirectoryResource, Homepage, HTMLResource
from .sitemap import SitemapResource

logger = logging.getLogger(__name__)


class Website(DirectoryResource):

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
        if os.path.basename(path) == '404.html':
            return HTMLResource(path)
        if os.path.basename(path) == 'index.html':
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
