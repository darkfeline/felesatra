"""frelia page module.

Contains resources for loading and rendering content pages.  Content pages are
pages that present an independent document of content, as opposed to a page
that aggregates content pages, like a listing of blog posts.

"""

import os

import frelia.descriptors
import frelia.enja
import frelia.fs


class PageReourceLoader:

    def __init__(self, page_loader, resource_class):
        self.page_loader = page_loader
        self.resource_class = resource_class


def load_pages(page_loader, page_dir):
    """Generate PageResource instances from a directory tree."""
    for filepath in frelia.fs.walk_files(page_dir):
        with open(filepath) as file:
            page = page_loader.load(file)
        relpath = os.path.relpath(filepath, page_dir)
        pagepath = os.path.splitext(relpath)[0]
        yield PageResource(pagepath, page)


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
        return os.path.join(build_dir, self.path, 'index.html')


class Page:

    """Represents a (web)page.

    A page consists of content and its associated metadata.  Much like files in
    a filesystem, a page doesn't contain information about its filename or
    path.

    """

    def __init__(self, document):
        self.document = document

    def __repr__(self):
        return '{classname}({env!r}, {document!r})'.format(
            classname=type(self).__name__,
            env=self.env,
            document=self.document)

    def render_page(self, env):
        """Render page."""
        template = self._get_template(env)
        return template.render(self._context)

    @classmethod
    def _copy_default_context(cls):
        return cls._DEFAULT_CONTEXT.copy()

    _DEFAULT_CONTEXT = {
        'template': 'base.html',
        'title': '',
    }

    def _get_template(self, env):
        """Jinja template."""
        return env.get_template(self._context['template'])

    @frelia.descriptors.CachedProperty
    def _context(self):
        """Jinja rendering context for this page."""
        context = self._copy_default_context()
        context.update(self.document.metadata)
        context['content'] = self.document.content
        return context


class PageLoader:

    """Loads pages."""

    def __init__(self, page_class, document_loader):
        self.page_class = page_class
        self.document_loader = document_loader

    def __repr__(self):
        return '{classname}({page_class!r}, {loader!r})'.format(
            classname=type(self).__name__,
            page_class=self.page_class,
            loader=self.document_loader)

    def load(self, file):
        """Make a Page instance from a file object."""
        document = self.document_loader.load(file)
        return self.page_class(document)
