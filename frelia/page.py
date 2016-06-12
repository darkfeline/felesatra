"""Frelia page module.

Contains resources for loading and rendering content pages.  Content pages are
pages that present an independent document of content, as opposed to a page
that aggregates content pages, like a listing of blog posts.

"""

import io
import os
from collections import namedtuple

import yaml

import frelia.descriptors
import frelia.fs


def load_pages(env, page_dir):
    """Generate PageResource instances from a directory tree."""
    for filepath in frelia.fs.walk_files(page_dir):
        with open(filepath) as file:
            page = Page.from_file(env, file)
        rel_path = os.path.relpath(filepath, page_dir)
        yield PageResource(rel_path, page)


class PageResource:

    """Represents a page resource for rendering.

    Contains the page itself and the path where the page would be built.

    """

    def __init__(self, path, page):
        self.path = path
        self.page = page

    def build(self, build_dir):
        """Build page into build_dir."""
        dst = self._get_dst_path(build_dir)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        with open(dst, 'w') as file:
            file.write(self.page.rendered_page)

    def _get_dst_path(self, build_dir):
        """Get the path of the file that this resource will build."""
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

    def __init__(self, env, metadata, content):
        self.env = env
        self.metadata = metadata
        self.content = content

    @classmethod
    def from_file(cls, env, file):
        """Make a Page instance from a file object."""
        page_file = cls._parse_page_file(file)
        metadata = cls._DEFAULT_METADATA.copy()
        metadata.update(yaml.load(page_file.frontmatter))
        return cls(env, metadata, page_file.content)

    _DEFAULT_METADATA = {
        'template': 'base.html',
        'title': '',
    }

    @staticmethod
    def _parse_page_file(file):
        frontmatter = io.StringIO()
        for line in file:
            if line.startswith('---'):
                break
            frontmatter.write(line)
        content = file.read()
        return PageFile(frontmatter.getvalue(), content)

    @frelia.descriptors.CachedProperty
    def rendered_page(self):
        """Return the rendered page."""
        template = self.env.get_template(self.metadata['template'])
        context = self.metadata.copy()
        context['content'] = self.rendered_content
        rendered_page = template.render(context)
        return rendered_page

    @frelia.descriptors.CachedProperty
    def rendered_content(self):
        """Render macros in content."""
        content_as_template = self.env.from_string(self.content)
        return content_as_template.render()

PageFile = namedtuple('PageFile', ['frontmatter', 'content'])
