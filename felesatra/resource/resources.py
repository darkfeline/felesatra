"""Website resources."""

import abc
import datetime
from datetime import timezone
import functools
from html.parser import HTMLParser
import logging
import pathlib
import shutil

from mir.frelia import enja

from felesatra import utils
from felesatra.resource.index import PageIndexEntry

logger = logging.getLogger(__name__)


class Resource(abc.ABC):
    """Resource."""

    @abc.abstractmethod
    def deploy(self, env, target):
        """Deploy resource using the given environment."""


class Indexable(abc.ABC):
    """Interface for indexable resource."""

    @abc.abstractmethod
    def get_index_entry(self):
        """Get index entry."""


class BaseFileResource(Resource):

    def __init__(self, resource_path):
        self._resource_path = pathlib.PurePath(resource_path)

    def deploy(self, env, target_path):
        logger.debug('Deploying %r to %r with %r', self, target_path, env)
        target_path = pathlib.Path(target_path) / self._resource_path
        target_path.mkdir(parents=True, exist_ok=True)
        self._deploy_file(env, target_path)

    @abc.abstractmethod
    def _deploy_file(self, env, target_path):
        """Deploy a file resource."""


class FileResource(BaseFileResource):

    """Represents a file resource.

    Subclasses that also deploy a file, but require different ways of deploying
    the file can override the _deploy_file() method.
    """

    def __init__(self, file_path, resource_path):
        super().__init__(resource_path)
        self._file_path = pathlib.Path(file_path)

    def _deploy_file(self, env, target_path):
        shutil.copy(self._file_path, target_path)


class HTMLResource(FileResource):

    """Represents an HTML file resource."""

    def __init__(self, file_path, resource_path):
        super().__init__(file_path, resource_path)

        with file_path.open() as file:
            document = enja.load(file)
        self._content = document.body

        self._meta = {
            'template': 'base.html',
            'title': '',
        }
        self._meta.update(document.header)

    def _render_content(self, env):
        """Render content only."""
        content_template = env.from_string(self.content)
        rendered_content = content_template.render()
        return rendered_content

    def _render_html(self, env):
        """Render the HTML with template."""
        # Render page content itself first (e.g., macros).
        content = self._render_content(env)

        # Render page content into template.
        template = env.get_template(self.meta['template'])
        context = {'content': content}
        context.update(self.meta)
        return template.render(context)

    def _deploy_file(self, env, target_path):
        with target_path.open('w') as file:
            file.write(self._render_html(env))


class _TextParser(HTMLParser):

    """HTML parser that extracts all text."""

    def reset(self):
        super().reset()
        self.text = []

    def handle_data(self, data):
        self.text.append(data)


class WebPageResource(HTMLResource, Indexable):

    """Web page resource.

    Similar to HTMLResource, but renders a web page into
    <page_name>/index.html, thus allowing it to be served with the URL
    <page_name>/.

    As a web page, this resource will also be added to the page index.
    """

    def __init__(self, file_path, resource_path):
        """Initialize instance."""
        super().__init__(file_path, resource_path)

        if 'published' not in self._meta and len(file_path.parents) >= 3:
            year, month, day = file_path.parts[-4:-1]
            self._meta['published'] = datetime.datetime(
                int(year), int(month), int(day))

    @property
    @functools.lru_cache(None)
    def _updated(self):
        """When resource was updated."""
        if 'modified' in self._meta:
            return self._meta['modified']
        else:
            return self._meta['published']

    def _render_summary(self, env):
        """Content summary.

        Returns a text summary of the resource content without HTML tags.
        """
        content = self._render_content(env)
        parser = _TextParser()
        parser.feed(content)
        summary_words = []
        for text in parser.text:
            words = text.split()
            summary_words.extend(words)
            if len(summary_words) > 200:
                break
        return ' '.join(summary_words)

    def get_index_entry(self, env):
        path = str(self._resource_path) + '/'
        url = utils.geturl(env, path)
        entry = PageIndexEntry(url, self.meta['title'])
        entry.published = self._meta.get('published')
        entry.updated = self._updated
        entry.category = self._meta.get('category')
        entry.tags = self._meta.get('tags', [])
        entry.summary = self._render_summary(env)
        entry.include_in_atom = self._meta.get('include_in_atom', True)
        return entry

    def _deploy_file(self, env, target_path):
        super()._deploy_file(env, target_path / 'index.html')


class HomePageResource(HTMLResource, Indexable):

    """Homepage resource.

    Similar to Webpage, but always renders into index.html in its target
    directory, suitable for the root URL path of a website.
    """

    @property
    @functools.lru_cache(None)
    def _updated(self):
        """When resource was updated."""
        if 'modified' in self._meta:
            return self._meta['modified']
        else:
            return self._meta['published']

    def get_index_entry(self, env):
        entry = PageIndexEntry('/', 'Feles Atra')
        entry.published = self._meta.get('published')
        entry.updated = self._updated
        entry.include_in_atom = False
        return entry


class AtomResource(BaseFileResource):

    """Atom feed resource."""

    def __init__(self, context, resource_path):
        super().__init__(resource_path)
        self._context = context

    def _deploy_file(self, env, target_path):
        # Make a copy of the Atom context.
        context = dict(self.context)
        # Set up Atom entries.
        entries = env.globals['page_index']
        entries = [
            entry.atom_entry()
            for entry in entries
            if entry.include_in_atom
        ]
        context['entries'] = entries
        # Calculate updated time for Atom feed.
        if entries:
            updated = max(entry.updated for entry in entries)
        else:
            updated = datetime.datetime.now(timezone.utc)
        context['updated'] = updated
        # Render Atom feed.
        template = env.get_template('atom.xml')
        content = template.render(context)
        with target_path.open('w') as file:
            file.write(content)


class SitemapResource(BaseFileResource):

    """Sitemap resource."""

    def _deploy_file(self, env, target_path):
        pages = env.globals['page_index']
        sitemap = [entry.sitemap_entry() for entry in pages]
        logger.debug('sitemap %r', sitemap)
        template = env.get_template('sitemap.xml')
        content = template.render({'sitemap': sitemap})
        with target_path.open('w') as file:
            file.write(content)
