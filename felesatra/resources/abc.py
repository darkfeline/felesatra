"""Resource ABCs."""

import logging
import os
from abc import ABC, abstractmethod

logger = logging.getLogger(__name__)


class Resource(ABC):

    """Represents a render-able resource."""

    @abstractmethod
    def render(self, env, target):
        """Render this resource into target.

        How the resource is rendered relative to the target depends on the
        resource.
        """
        logger.debug('Render %r to %s', self, target)


class FileResource(Resource):

    """Represents a resource file.

    Specifically, either a file, directory, or other object on the file system
    with a path.
    """

    def __init__(self, path):
        if not self.valid_resource(path):
            raise ValueError('Invalid {}: {}'.format(self.__class__, path))
        self.path = path

    def __repr__(self):
        return '{}({})'.format(self.__class__.__name__, self.path)

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
