"""Web page resource."""

import logging
import os

import yaml

from felesatra import utils

from . import base
from .abc import FileResource
from .sitemap import SitemapURL

logger = logging.getLogger(__name__)


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


class HTMLResource(FileResource):

    """HTML resource.

    This resource will render an HTML file with full templating into its target
    file.

    """

    def __init__(self, path):
        super().__init__(path)

        with open(self.path) as file:
            frontmatter = []
            for line in file:
                if line.rstrip() == '---':
                    break
                frontmatter.append(line)

            self.content = file.read().lstrip()

        self.meta = {
            'template': 'base.html',
            'title': '',
            'published': None,
            'modified': None,
        }
        self.meta.update(yaml.load(''.join(frontmatter)))

    def render_html(self, env):
        """Render the HTML only."""
        # Render page content itself first (e.g., macros).
        content_template = env.from_string(self.content)
        content = content_template.render()

        # Render page content into template.
        template = env.get_template(self.meta['template'])
        context = {'content': content}
        context.update(self.meta)
        return template.render(context)

    def render(self, env, target):
        """Render this resource into target."""
        with open(target, 'w') as file:
            file.write(self.render_html(env))


class Webpage(HTMLResource):

    """Web page resource.

    Similar to HTMLResource, but renders a web page into
    <page_name>/index.html, thus allowing it to be served with the URL
    <page_name>/.

    As a web page, this resource will also be added to the sitemap.

    """

    @staticmethod
    def rendered_path(path):
        """Get relative path that page would be rendered as.

        Example: 'foo/bar.html' -> 'foo/bar/'

        """
        return os.path.splitext(path)[0] + '/'

    def walk(self, env):
        """Load information about this resource."""
        path = self.rendered_path(self.path)
        env.globals['sitemap'].append(
            SitemapURL(
                utils.geturl(env, path),
                self.meta.get('modified'),
                None,
                None))

    def render(self, env, target):
        """Render this resource into target."""
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
