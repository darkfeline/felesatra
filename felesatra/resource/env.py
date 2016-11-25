"""Rendering environment."""

import abc


class Environment(abc.ABC):

    """Rendering environment."""

    @abc.abstractmethod
    def get_target_path(self, path):
        """Return the rendering target path."""
