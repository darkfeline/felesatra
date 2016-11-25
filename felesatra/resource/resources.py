"""Website resources."""

import abc
import collections
import logging

logger = logging.getLogger(__name__)


class Resource(abc.ABC):
    """Interface for rendering."""

    @abc.abstractmethod
    def render(self, env):
        """Render resource using the given environment."""


class Indexable(abc.ABC):
    """Interface for indexable resource."""

    @abc.abstractmethod
    def get_index_entry(self):
        """Get index entry."""


class ResourceDir(collections.UserDict):
    """Represents a directory of resources."""
