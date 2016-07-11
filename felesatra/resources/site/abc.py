"""Site-specific resource ABCs."""

from abc import abstractmethod

from felesatra.resources.abc import Resource


class SiteResource(Resource):

    """Represents an indexable site resource."""

    @abstractmethod
    def index(self, env):
        """Index this resource and store its metadata in the environment."""
