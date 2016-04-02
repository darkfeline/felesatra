"""This module contains classes for accessing resources."""

import os
import logging
import shutil

# pylint: disable=too-few-public-methods

logger = logging.getLogger(__name__)


class Resource:

    """Represents a render-able resource."""

    def render(self, env, target):
        # pylint: disable=no-self-use,unused-argument
        """Render this resource into target."""
        pass


class FileResource:

    """Represents a resource file with a path."""

    def __init__(self, path):
        self.path = path

    @property
    def filename(self):
        """Filename of resource file."""
        return os.path.basename(self.path)

    def render(self, env, target):
        # pylint: disable=unused-argument
        """Render this resource into target."""
        shutil.copy(self.path, target)


class DirectoryResource(FileResource):

    """Represents a resource that is a directory.

    Can be iterated for all resources:

    >>> for path, resource in DirectoryResource('foo'):
    ...     resource.render(path)

    """

    def __iter__(self):
        for filename in os.listdir(self.path):
            yield filename, self.load(self.getpath(filename))

    def getpath(self, path, *paths):
        """Get path relative to resource directory."""
        return os.path.join(self.path, path, *paths)

    def render(self, env, target):
        """Render this resource into target."""
        os.makedirs(os.path.join(target, self.filename), exist_ok=True)
        for path, resource in self:
            logger.debug('Render %s, %s', path, resource)
            resource.render(env, os.path.join(target, path))

    @classmethod
    def load(cls, path):
        """Load resource."""
        if os.path.isdir(path):
            return DirectoryResource(path)
        elif os.path.isfile(path):
            return FileResource(path)
        else:
            raise LoadingError('Unknown file %s', path)


class RenderError(Exception):
    """Generic error when rendering."""


class LoadingError(Exception):
    """Generic error when loading resource."""
