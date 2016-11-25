import abc


class ResourceLoader(abc.ABC):
    """Interface for resource loader."""

    @abc.abstractmethod
    def __call__(self, path):
        """Load resources."""


class WebsiteLoader():
    """ResourceLoader for a website."""
