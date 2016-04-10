"""Web page resources.

These resources are for rendering webpage resources, which are distinct from
HTML resources.  They contain various handling to integrate with the larger
website.

"""

import functools
import logging
import os
from html.parser import HTMLParser

from felesatra import utils
from felesatra.fields import BoolField, DateTimeField, ListField, StringField
from felesatra.resources.html import HTMLResource

from . import atom
from .abc import SiteResource
from .sitemap import SitemapURL

logger = logging.getLogger(__name__)


class PageIndexEntry:

    """Page index entry used for many things.

    For example: sitemap, Atom feed, site index

    """

    # pylint: disable=too-many-instance-attributes

    __slots__ = ['__weakref__', 'href', 'title']

    def __init__(self, href, title):
        self.href = href
        self.title = title

    published = DateTimeField()
    updated = DateTimeField()
    category = StringField()
    tags = ListField()

    summary = StringField()

    include_in_sitemap = BoolField(True)
    include_in_atom = BoolField(True)

    def sitemap_entry(self):
        """Return sitemap entry of page index."""
        return SitemapURL(self.href, self.updated, None, None)

    def atom_entry(self):
        """Return Atom entry of page index."""
        entry = atom.Entry(self.href, self.title, self.updated)
        entry.add_link(self.href, 'alternate', 'text/html')
        entry.summary = self.summary
        entry.published = self.published
        if self.category:
            entry.add_category(self.category, 'category')
        for tag in self.tags:
            entry.add_category(tag, 'tag')
        return entry


class _TextParser(HTMLParser):

    """HTML parser that extracts all text."""

    # pylint: disable=abstract-method

    def reset(self):
        super().reset()
        self.text = []

    def handle_data(self, data):
        self.text.append(data)


class Webpage(HTMLResource, SiteResource):

    """Web page resource.

    Similar to HTMLResource, but renders a web page into
    <page_name>/index.html, thus allowing it to be served with the URL
    <page_name>/.

    As a web page, this resource will also be added to the page index.

    """

    @property
    @functools.lru_cache(None)
    def updated(self):
        """When resource was updated."""
        if 'modified' in self.meta:
            return self.meta['modified']
        else:
            return self.meta['published']

    def render_summary(self, env):
        """Content summary.

        Returns a text summary of the resource content without HTML tags.

        """
        content = self.render_content(env)
        parser = _TextParser()
        parser.feed(content)
        summary_words = []
        for text in parser.text:
            words = text.split()
            summary_words.extend(words)
            if len(summary_words) > 200:
                break
        return ' '.join(summary_words)

    @staticmethod
    @functools.lru_cache()
    def rendered_path(path):
        """Get path that page would be rendered as.

        Example: 'foo/bar.html' -> 'foo/bar/'

        """
        return os.path.splitext(path)[0] + '/'

    def index(self, env):
        """Load information about this resource."""
        path = self.rendered_path(self.path)
        url = utils.geturl(env, path)
        entry = PageIndexEntry(url, self.meta['title'])
        entry.published = self.meta.get('published')
        entry.updated = self.updated
        entry.category = self.meta.get('category')
        entry.tags = self.meta.get('tags', [])
        entry.summary = self.render_summary(env)
        entry.include_in_atom = self.meta.get('include_in_atom', True)
        env.globals['page_index'].append(entry)

    def render(self, env, target):
        """Render this resource into target."""
        logger.debug('Render %r to %s', self, target)
        target = self.rendered_path(target)
        os.makedirs(target, exist_ok=True)
        target = os.path.join(target, 'index.html')
        super().render(env, target)


class Homepage(Webpage):

    """Homepage resource.

    Similar to Webpage, but always renders into index.html in its target
    directory, suitable for the root URL path of a website.

    """

    def index(self, env):
        """Load information about this resource."""
        entry = PageIndexEntry('/', 'Feles Atra')
        entry.published = self.meta.get('published')
        entry.updated = self.updated
        entry.include_in_atom = False
        env.globals['page_index'].append(entry)

    def render(self, env, target):
        """Render this resource into target."""
        logger.debug('Render %r to %s', self, target)
        target = os.path.dirname(target)
        super().render(env, target)
