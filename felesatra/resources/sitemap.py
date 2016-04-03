"""Sitemap resources."""

import logging
from collections import namedtuple

from .abc import Resource

logger = logging.getLogger(__name__)

SitemapURL = namedtuple(
    'SitemapURL',
    ['loc', 'lastmod', 'changefreq', 'priority'])


class SitemapResource(Resource):

    """Sitemap resource."""

    # pylint: disable=too-few-public-methods

    def __init__(self, sitemap):
        self.sitemap = sitemap

    def render(self, env, target):
        """Render this resource into target."""
        logger.debug('sitemap %r', self.sitemap)
        template = env.get_template('sitemap.xml')
        content = template.render({'sitemap': self.sitemap})
        with open(target, 'w') as file:
            file.write(content)
