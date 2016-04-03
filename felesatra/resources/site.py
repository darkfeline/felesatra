"""Website resource."""

import logging
import os

from .base import DirectoryResource
from .page import HTMLResource, Webpage

logger = logging.getLogger(__name__)


class Website(DirectoryResource):

    """Website for rendering."""

    @classmethod
    def load(cls, path):
        """Load resource."""
        if os.path.basename(path) == 'index.html':
            return HTMLResource(path)
        elif path.endswith('.html'):
            return Webpage(path)
        else:
            return super().load(path)
