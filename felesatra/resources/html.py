"""HTML resource.

This module provides HTMLResource for rendering HTML files with Jinja
templating.  However, HTMLResource does not support any of the features needed
for a web page as part of a web site, such as rendering to index.html,
registering in the sitemap or Atom feeds, etc.

"""

import yaml

from felesatra.xhtml import XHTMLParser

from .abc import FileResource


class HTMLResource(FileResource):

    """HTML resource.

    This resource will render an HTML file with full templating into its target
    file.

    The HTML resource should have a YAML front matter.

    HTMLResource guarantees only template and title keys in meta.

    Attributes:
        meta: Parsed from front matter
        content: HTML content sans front matter

    """

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
        }
        self.meta.update(yaml.load(''.join(frontmatter)))

    @classmethod
    def valid_resource(cls, path):
        return path.endswith('.html')

    def render_content(self, env):
        """Render content only."""
        content_template = env.from_string(self.content)
        rendered_content = content_template.render()
        parser = XHTMLParser()
        parser.feed(rendered_content)
        return parser.get_text()

    def render_html(self, env):
        """Render the HTML with template."""
        # Render page content itself first (e.g., macros).
        content = self.render_content(env)

        # Render page content into template.
        template = env.get_template(self.meta['template'])
        context = {'content': content}
        context.update(self.meta)
        return template.render(context)

    def render(self, env, target):
        """Render this resource into target."""
        super().render(env, target)
        with open(target, 'w') as file:
            file.write(self.render_html(env))