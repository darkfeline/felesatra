"""This module contains classes for accessing resources."""

import os


class Resource:

    """Represents a resource."""

    def render(self):
        # pylint: disable=no-self-use
        """Render this resource."""
        return ''


class DirectoryResource(Resource):

    """Represents a resource that is a directory.

    Can be iterated for all resources:

    >>> for resource in DirectoryResource('foo'):
    ...     resource.render()

    """

    def __init__(self, path):
        self.path = path

    def getpath(self, path, *paths):
        """Get path relative to page directory."""
        return os.path.join(self.path, path, *paths)

    def __iter__(self):
        return iter(())
