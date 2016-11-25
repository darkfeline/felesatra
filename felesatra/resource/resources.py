"""Website resources."""

import abc
import logging
import pathlib
import shutil

from mir.frelia import enja

logger = logging.getLogger(__name__)


class Resource(abc.ABC):
    """Resource."""

    @abc.abstractmethod
    def deploy(self, env):
        """Deploy resource using the given environment."""


class Indexable(abc.ABC):
    """Interface for indexable resource."""

    @abc.abstractmethod
    def get_index_entry(self):
        """Get index entry."""


class FileResource(Resource):

    """Represents a file resource.

    Subclasses that also deploy a file, but require different ways of deploying
    the file can override the _deploy_file() method.
    """

    def __init__(self, file_path, resource_path):
        self._file_path = pathlib.Path(file_path)
        self._resource_path = pathlib.PurePath(resource_path)

    def deploy(self, env):
        logger.debug('Deploying %r with %r', self, env)
        target_path = env.get_target_path() / self._resource_path
        target_path.mkdir(parents=True, exist_ok=True)
        self._deploy_file(env, target_path)

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


class WebPageResource(HTMLResource, Indexable):
    ...
