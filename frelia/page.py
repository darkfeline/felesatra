"""Frelia page module.

Contains resources for loading and rendering content pages.  Content pages are
pages that present an independent document of content, as opposed to a page
that aggregates content pages, like a listing of blog posts.

"""
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

    Contains the page itself and the path where the page would be built.

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
        dst = self._get_dst_path(build_dir)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        with open(dst, 'w') as file:
            file.write(self.page.render(env))

    def _get_dst_path(self, build_dir):
        return os.path.join(
            build_dir,
            os.path.dirname(self.path),
            os.path.splitext(self.path)[0],
            'index.html',
        )


class Page:

    """Represents a (web)page.

    A page consists of content and its associated metadata.  Much like files in
    a filesystem, a page doesn't contain information about its filename or
    path.

    """

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content

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

    @staticmethod
    def _parse_frontmatter(file):
        frontmatter = []
        for line in file:
            if line.startswith('---'):
                break
            frontmatter.append(line)
        content = file.read()
        return ''.join(frontmatter), content

    def render(self, env):
        """Return the rendered page."""
        template = env.get_template(self.metadata['template'])
        context = self.metadata.copy()
        context['content'] = self._render_content(env)
        rendered_page = template.render(context)
        return rendered_page

    def _render_content(self, env):
        """Render macros in content."""
        content_as_template = env.from_string(self.content)
        return content_as_template.render()
