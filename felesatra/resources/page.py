"""Web page resource."""

import functools
import logging
import os
from collections import namedtuple
from html.parser import HTMLParser

from felesatra import utils

from . import base
from .atom import Entry, Link
from .html import HTMLResource
from .sitemap import SitemapURL

logger = logging.getLogger(__name__)

Page = namedtuple('Page', ['href', 'title', 'published', 'updated', 'summary'])


class DirectoryResource(base.DirectoryResource):

    """Directory resource extended with webpage support."""

    @classmethod
    def load(cls, path):
        """Load resource."""
        if path.endswith('.html'):
            return Webpage(path)
        elif os.path.isdir(path):
            return DirectoryResource(path)
        else:
            return super().load(path)


class TextParser(HTMLParser):

    """HTML parser that extracts all text."""

    # pylint: disable=abstract-method

    def __init__(self):
        super().__init__()
        self.text = []

    def handle_data(self, data):
        self.text.append(data)


class Webpage(HTMLResource):

    """Web page resource.

    Similar to HTMLResource, but renders a web page into
    <page_name>/index.html, thus allowing it to be served with the URL
    <page_name>/.

    As a web page, this resource will also be added to the sitemap.

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
        parser = TextParser()
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

    def walk(self, env):
        """Load information about this resource."""
        path = self.rendered_path(self.path)
        url = utils.geturl(env, path)
        env.globals['sitemap'].append(
            SitemapURL(
                url,
                self.meta.get('modified'),
                None,
                None))
        env.globals['atom_entries'].append(
            Entry(
                url,
                self.meta['title'],
                self.updated,
                [Link(path, 'alternate', 'text/html')],
                self.render_summary(env),
                self.meta.get('published')))
        env.globals['pages'].append(
            Page(
                url,
                self.meta['title'],
                self.meta['published'],
                self.updated,
                None))

    def render(self, env, target):
        """Render this resource into target."""
        target = self.rendered_path(target)
        os.makedirs(target, exist_ok=True)
        with open(os.path.join(target, 'index.html'), 'w') as file:
            file.write(self.render_html(env))


class Homepage(Webpage):

    """Homepage resource.

    Similar to Webpage, but always renders into index.html in its target
    directory, suitable for the root URL path of a website.

    """

    def walk(self, env):
        """Load information about this resource."""
        env.globals['sitemap'].append(
            SitemapURL(
                utils.geturl(env, '/'),
                self.meta.get('modified'),
                None,
                None))

    def render(self, env, target):
        """Render this resource into target."""
        target = os.path.dirname(target)
        super().render(env, target)
