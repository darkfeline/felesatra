"""Resource loaders."""

import abc
import pathlib

from mir.resource import resources


class ResourceLoader(abc.ABC):

    """Interface for resource loader."""

    @abc.abstractmethod
    def __call__(self, path):
        """Load resources.

        path is a path-like object.
        """


class WebsiteDirectoryLoader(ResourceLoader):

    """ResourceLoader for directories under a website."""

    def __call__(self, path, resource_path=pathlib.PurePath()):
        dirpath = pathlib.Path(path)
        for subpath in dirpath.iterdir():
            yield from self._load_resources(subpath, resource_path)

    def _load_resources(self, path, resource_path):
        """Load resources.

        path is a Path object.
        """
        if path.is_dir():
            yield from self(path, resource_path / path.name)
        else:
            yield self._load_resource(path, resource_path)

    def _load_resource(self, path, resource_path):
        if path.suffix == '.html':
            return resources.WebPageResource(path, resource_path / path.stem)
        else:
            return resources.FileResource(path, resource_path / path.name)


class WebsiteLoader(WebsiteDirectoryLoader):

    """ResourceLoader for a website."""
