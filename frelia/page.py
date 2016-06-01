import os

import yaml

from .fs import walk_files


def load_pages(page_dir):
    for filepath in walk_files(page_dir):
        yield PageResource.from_pathname(filepath, page_dir)


def build_pages(env, pages):
    for page in pages:
        page.build(env)


class PageResource:

    """Represents a page resource for rendering.

    Contains the page itself and the path where the page would be rendered.

    """

    def __init__(self, path, page):
        self.path = path
        self.page = page

    @classmethod
    def from_pathname(cls, pathname, start):
        with open(pathname) as file:
            return cls(os.path.relpath(pathname, start),
                       Page.from_file(file))

    def build(self, env):
        build_dir = env.globals['build_dir']
        dst = os.path.join(build_dir, self.path)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        with open(dst, 'w') as file:
            file.write(self.page.render(env))


class Page:

    """Represents a (web)page.

    A page consists of content and its associated metadata.

    """

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content

    @staticmethod
    def _parse_frontmatter(file):
        frontmatter = []
        for line in file:
            if line.startswith('---'):
                break
            frontmatter.append(line)
        content = file.read()
        return ''.join(frontmatter), content

    @classmethod
    def from_file(cls, file):
        """Make a Page instance from a file object."""
        frontmatter, content = cls._parse_frontmatter(file)

        metadata = {
            'template': 'base.html',
            'title': '',
        }
        metadata.update(yaml.load(frontmatter))
        return cls(metadata, content)

    def get_context(self):
        """Get context for rendering"""
        context = self.metadata.copy()
        context['content'] = self.content
        return context

    def render(self, env):
        template = env.get_template(self.metadata['template'])
        context = self.get_context()
        rendered_page = template.render(context)
        return rendered_page
