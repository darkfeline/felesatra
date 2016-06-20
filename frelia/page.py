"""frelia page module.

Contains resources for loading and rendering content pages.  Content pages are
pages that present an independent document of content, as opposed to a page
that aggregates content pages, like a listing of blog posts.

"""

import os

import frelia.descriptors
import frelia.enja
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
        root, ext = os.path.splitext(self.path)
        return os.path.join(build_dir, root, 'index' + ext)


class Page:

    """Represents a (web)page.

    A page consists of content and its associated metadata.  Much like files in
    a filesystem, a page doesn't contain information about its filename or
    path.

    """

    def __init__(self, env, document):
        self.env = env
        self.document = document

    def __repr__(self):
        return '{classname}({env!r}, {document!r})'.format(
            classname=type(self).__name__,
            env=self.env,
            document=self.document)

    @classmethod
    def from_file(cls, env, file):
        """Make a Page instance from a file object."""
        rendered_content = cls._render_content(env, file.read())
        document = frelia.enja.EnjaDocument.from_string(rendered_content)
        return cls(env, document)

    @property
    def metadata(self):
        return self.document.metadata

    @property
    def content(self):
        return self.document.content

    @frelia.descriptors.CachedProperty
    def rendered_page(self):
        """The rendered page."""
        template = self.env.get_template(self.metadata['template'])
        rendered_page = template.render(self._context)
        return rendered_page

    @classmethod
    def _get_default_metadata(cls):
        return cls._DEFAULT_METADATA.copy()

    _DEFAULT_METADATA = {
        'template': 'base.html',
        'title': '',
    }

    @property
    def _context(self):
        """Jinja rendering context for this page."""
        context = self._get_default_metadata()
        context.update(self.metadata)
        context['content'] = self.content
        return context

    @staticmethod
    def _render_content(env, raw_content):
        """Render macros in content."""
        content_as_template = env.from_string(raw_content)
        return content_as_template.render()
