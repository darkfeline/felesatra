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
        """Render this resource into target."""
        logger.debug('Render file %s %s', self, target)
        shutil.copy(self.path, target)


class BaseDirectoryResource(FileResource):

    """Represents a directory resource.

    Can be iterated for all resources:

    >>> for path, resource in DirectoryResource('foo'):
    ...     resource.render(path)

    path is relative to the directory.

    This resource renders the entries in the directory into the target, but not
    the directory itself.

    """

    def __iter__(self):
        for filename in os.listdir(self.path):
            yield filename, self.load(self.getpath(filename))

    def getpath(self, path, *paths):
        """Get path relative to resource directory."""
        return os.path.join(self.path, path, *paths)

    def render(self, env, target):
        """Render this resource into target."""
        logger.debug('Render dir %s %s', self, target)
        for path, resource in self:
            logger.debug('Render dir entry %s %s', path, resource)
            resource.render(env, os.path.join(target, path))

    @classmethod
    def load(cls, path):
        """Load resource."""
        if os.path.isdir(path):
            return DirectoryResource(path)
        elif os.path.isfile(path):
            return SimpleFileResource(path)
        else:
            raise LoadingError('Unknown file %s', path)


class DirectoryResource(BaseDirectoryResource):

    """Represents a directory resource.

    Unlike BaseDirectoryResource, this will render the directory itself to the
    target, then render its entries into the newly created directory.

    """

    def render(self, env, target):
        """Render this resource into target."""
        new_target = os.path.join(target, self.filename)
        os.makedirs(new_target, exist_ok=True)
        super().render(env, new_target)


class RenderError(Exception):
    """Generic error when rendering."""


class LoadingError(Exception):
    """Generic error when loading resource."""
