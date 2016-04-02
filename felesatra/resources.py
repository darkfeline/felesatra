"""This module contains classes for accessing resources."""

import os
import shutil


# pylint: disable=too-few-public-methods


class Resource:

    """Represents a render-able resource."""

    def render(self, target):
        # pylint: disable=no-self-use,unused-argument
        """Render this resource into target."""
        pass


class FileResource:

    """Represents a resource file with a path."""

    def __init__(self, path):
        self.path = path


class SimpleFileResource(FileResource):

    """Represents a resource file that will be rendered as-is."""

    def render(self, target):
        """Render this resource into target."""
        shutil.copy(self.path, target)


class DirectoryResource(FileResource):

    """Represents a resource that is a directory.

    Can be iterated for all resources:

    >>> for path, resource in DirectoryResource('foo'):
    ...     resource.render(path)

    """

    def getpath(self, path, *paths):
        """Get path relative to resource directory."""
        return os.path.join(self.path, path, *paths)

    def __iter__(self):
        return iter(())

    def render(self, target):
        """Render this resource into target."""
        for path, resource in self:
            resource.render(os.path.join(target, path))


class RenderError(Exception):
    """Generic error when rendering."""
