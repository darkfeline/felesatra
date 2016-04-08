"""Resource ABCs"""

# pylint: disable=abstract-method
# pylint: disable=too-few-public-methods

import os
from abc import ABC, abstractmethod


class Resource(ABC):

    """Represents a render-able resource."""

    @abstractmethod
    def walk(self, env):
        """Load information about this resource into the environment."""

    @abstractmethod
    def render(self, env, target):
        """Render this resource into target.

        How the resource is rendered relative to the target depends on the
        resource.

        """


class FileResource(Resource):

    """Represents a resource file.

    Specifically, either a file, directory, or other object on the file system
    with a path.

    """

    def __init__(self, path):
        if not self.valid_resource(path):
            raise ValueError('Invalid {}: {}'.format(self.__class__, path))
        self.path = path

    @classmethod
    @abstractmethod
    def valid_resource(cls, path):
        """Check if the given path is valid for this resource.

        This should return a true value if the path is valid, and a false value
        otherwise.  The exact return type and value is up to the implementing
        class.

        """


    @property
    def filename(self):
        """Filename of the resource file."""
        return os.path.basename(self.path)
