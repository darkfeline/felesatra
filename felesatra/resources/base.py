"""This module contains classes for accessing resources."""

import os
import logging
import shutil

from .abc import FileResource

logger = logging.getLogger(__name__)


class SimpleFileResource(FileResource):

    """Represents a simple resource file.

    Renders the resource file by copying it to the target.

    """

    def render(self, env, target):
        # pylint: disable=unused-argument
        """Render this resource to target."""
        logger.debug('Render file %r %r', self, target)
        shutil.copy(self.path, target)


class DirectoryResource(FileResource):

    """Represents a directory resource.

    Can be iterated for all resources:

    >>> import tempfile
    >>> with tempfile.TemporaryDirectory() as d:
    ...     for path, resource in DirectoryResource(d):
    ...         resource.render(path)

    path is relative to the directory.

    """

    def __iter__(self):
        for filename in os.listdir(self.path):
            yield filename, self.load(self.getpath(filename))

    def getpath(self, path, *paths):
        """Get path relative to resource directory."""
        return os.path.join(self.path, path, *paths)

    def render(self, env, target):
        """Render this resource into target."""
        logger.debug('Render dir %r %r', self, target)
        os.makedirs(target, exist_ok=True)
        for path, resource in self:
            logger.debug('Render dir entry %r %r', path, resource)
            resource.render(env, os.path.join(target, path))

    @classmethod
    def load(cls, path):
        """Load resource."""
        if os.path.isdir(path):
            return DirectoryResource(path)
        elif os.path.isfile(path):
            return SimpleFileResource(path)
        else:
            raise LoadingError('Unknown file %r', path)


class RenderError(Exception):
    """Generic error when rendering."""


class LoadingError(Exception):
    """Generic error when loading resource."""
