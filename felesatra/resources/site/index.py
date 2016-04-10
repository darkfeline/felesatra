"""Site index resources."""

import logging
from collections import namedtuple

from felesatra.resources.abc import Resource

logger = logging.getLogger(__name__)

SitemapURL = namedtuple(
    'SitemapURL',
    ['loc', 'lastmod', 'changefreq', 'priority'])


class SitemapResource(Resource):

    """Sitemap resource."""

    def walk(self, env):
        pass

    def render(self, env, target):
        """Render this resource into target."""
        sitemap = env.globals['sitemap']
        logger.debug('sitemap %r', sitemap)
        template = env.get_template('sitemap.xml')
        content = template.render({'sitemap': sitemap})
        with open(target, 'w') as file:
            file.write(content)
