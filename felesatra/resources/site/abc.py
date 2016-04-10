"""Site-specific resource ABCs."""

# pylint: disable=abstract-method
# pylint: disable=too-few-public-methods

from abc import abstractmethod

from felesatra.resources.abc import Resource


class SiteResource(Resource):

    """Represents an indexable site resource."""

    @abstractmethod
    def index(self, env):
        """Index this resource and store its metadata in the environment."""
