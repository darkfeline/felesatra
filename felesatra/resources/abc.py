"""Resource ABCs"""

import os
from abc import ABC, abstractmethod


# pylint: disable=abstract-method
# pylint: disable=too-few-public-methods


class Resource(ABC):

    """Represents a render-able resource."""

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
        self.path = path

    @property
    def filename(self):
        """Filename of the resource file."""
        return os.path.basename(self.path)
