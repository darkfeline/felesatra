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
        return os.path.join(build_dir, self.path, 'index.html')


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

    @frelia.descriptors.CachedProperty
    def rendered_page(self):
        """The rendered page."""
        return self._template.render(self._context)

    @classmethod
    def _copy_default_context(cls):
        return cls._DEFAULT_CONTEXT.copy()

    _DEFAULT_CONTEXT = {
        'template': 'base.html',
        'title': '',
    }

    @frelia.descriptors.CachedProperty
    def _template(self):
        """Jinja template."""
        return self.env.get_template(self._context['template'])

    @frelia.descriptors.CachedProperty
    def _context(self):
        """Jinja rendering context for this page."""
        context = self._copy_default_context()
        context.update(self.document.metadata)
        context['content'] = self.document.content
        return context


class PageLoader:

    _page_class = Page

    def __init__(self, env, document_loader):
        self.env = env
        self.document_loader = document_loader

    def __repr__(self):
        return '{classname}({env!r}, {loader!r})'.format(
            classname=type(self).__name__,
            env=self.env,
            loader=self.document_loader)

    def load(self, file):
        """Make a Page instance from a file object."""
        document = self.document_loader.load(file)
        return self._page_class(self.env, document)
