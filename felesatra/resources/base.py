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

    @classmethod
    def valid_resource(cls, path):
        return os.path.isfile(path)

    def render(self, env, target):
        # pylint: disable=unused-argument
        """Render this resource to target."""
        super().render(env, target)
        shutil.copy(self.path, target)


class DirectoryResource(FileResource):

    """Represents a directory resource.

    This resource will attempt to load resources from the files (regular and
    special) from its directory.  It attempts to load resources according to
    the classes returned from the resource_classes() class method.  Subclasses
    can extend resource_classes() or load() to modify resource loading.

    """

    def __init__(self, path):
        super().__init__(path)
        self.entries = dict((path, resource) for path, resource in self.iter_resources())

    def __iter__(self):
        for path, resource, in self.entries.items():
            yield path, resource

    @classmethod
    def valid_resource(cls, path):
        return os.path.isdir(path)

    def iter_resources(self):
        """Iterate over resources loaded from directory."""
        for filename in os.listdir(self.path):
            yield filename, self.load(os.path.join(self.path, filename))

    @classmethod
    def resource_classes(cls):
        """Resources that can be loaded.

        Returns an iterable.

        """
        yield DirectoryResource
        yield SimpleFileResource

    @classmethod
    def load(cls, path):
        """Load resource."""
        for resource_class in cls.resource_classes():
            if resource_class.valid_resource(path):
                return resource_class(path)
        raise LoadingError('No valid resources for {}'.format(path))

    def render(self, env, target):
        """Render this resource into target."""
        super().render(env, target)
        os.makedirs(target, exist_ok=True)
        for path, resource in self:
            resource.render(env, os.path.join(target, path))


class RenderError(Exception):
    """Generic error when rendering."""


class LoadingError(Exception):
    """Generic error when loading resource."""
