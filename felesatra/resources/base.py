"""This module contains classes for accessing resources."""

import logging
import os
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

    def walk(self, env):
        pass


class DirectoryResource(FileResource):

    """Represents a directory resource.

    path is relative to the directory.

    """

    def __init__(self, path):
        super().__init__(path)
        self.entries = dict((path, resource) for path, resource in self.iter_entries())

    def __iter__(self):
        for path, resource, in self.entries.items():
            yield path, resource

    def iter_entries(self):
        """Iterate over resources in directory."""
        for filename in os.listdir(self.path):
            yield filename, self.load(os.path.join(self.path, filename))

    @classmethod
    def load(cls, path):
        """Load resource."""
        if os.path.isdir(path):
            return DirectoryResource(path)
        elif os.path.isfile(path):
            return SimpleFileResource(path)
        else:
            raise LoadingError('Unknown file %r', path)

    def walk(self, env):
        # pylint: disable=unused-variable
        for path, resource in self:
            resource.walk(env)

    def render(self, env, target):
        """Render this resource into target."""
        logger.debug('Render dir %r %r', self, target)
        os.makedirs(target, exist_ok=True)
        for path, resource in self:
            logger.debug('Render dir entry %r %r', path, resource)
            resource.render(env, os.path.join(target, path))


class RenderError(Exception):
    """Generic error when rendering."""


class LoadingError(Exception):
    """Generic error when loading resource."""
