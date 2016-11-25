import pathlib


class PageIndex:
    """Page index."""

    def __init__(self):
        """Initialize instance."""
        self._pages = []

    def index(self):
        ...

    def index_directory(self, dirpath):
        ...


class EnvPageIndex:
    """Page index kept in Jinja environment."""

    def __init__(self, env):
        self._env = env
        env.globals['page_index'] = []


def index(src, env):
    """Index webpages in src."""
    src = pathlib.Path(src)
    page_index = EnvPageIndex(env)
