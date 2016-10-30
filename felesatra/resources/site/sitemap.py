"""Sitemap resources."""

import logging
from collections import namedtuple

from felesatra.resources.abc import Resource

logger = logging.getLogger(__name__)

SitemapURL = namedtuple(
    'SitemapURL',
    ['loc', 'lastmod', 'changefreq', 'priority'])


class SitemapResource(Resource):

    """Sitemap resource."""

    # pylint: disable=too-few-public-methods

    def __repr__(self):
        return "SitemapResource()"

    def render(self, env, target):
        """Render this resource into target."""
        super().render(env, target)
        pages = env.globals['page_index']
        sitemap = [entry.sitemap_entry() for entry in pages]
        logger.debug('sitemap %r', sitemap)
        template = env.get_template('sitemap.xml')
        content = template.render({'sitemap': sitemap})
        with open(target, 'w') as file:
            file.write(content)
