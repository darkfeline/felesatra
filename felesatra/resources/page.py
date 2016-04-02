"""Web page resource."""

import logging
import os

import yaml

from .abc import FileResource

logger = logging.getLogger(__name__)


class HTMLResource(FileResource):

    """HTML resource for rendering."""

    def __init__(self, path):
        super().__init__(path)

        with open(self.path) as file:
            frontmatter = []
            for line in file:
                if line.rstrip() == '---':
                    break
                frontmatter.append(line)

            self.content = file.read()

        self.meta = {
            'template': 'base.html',
            'title': '',
            'published': None,
            'modified': None,
        }
        self.meta.update(yaml.load(''.join(frontmatter)))
        logger.debug('with context %s', self.meta)

    def render_html(self, env):
        """Render the HTML only."""
        # Render page content itself first (e.g., macros).
        content_template = env.from_string(self.content)
        content = content_template.render()

        # Render page content into template.
        template = env.get_template(self.meta['template'])
        context = {
            'title': self.meta['title'],
            'content': content,
            'published': self.meta['published'],
            'modified': self.meta['modified'],
        }
        return template.render(context)

    def render(self, env, target):
        """Render this resource into target."""
        with open(target, 'w') as file:
            file.write(self.render_html(env))


class Webpage(HTMLResource):

    """Web page resource for rendering.

    Will render into <page_name>/index.html.

    """

    def render(self, env, target):
        """Render this resource into target."""
        dirtarget = os.path.join(target.rstrip('.html'))
        os.makedirs(dirtarget, exist_ok=True)
        with open(os.path.join(dirtarget, 'index.html'), 'w') as file:
            file.write(self.render_html(env))
