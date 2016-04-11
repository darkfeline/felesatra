"""Make Jinja environment."""

from jinja2 import Environment, PackageLoader

from . import filters


def make_env():
    """Get Jinja environment."""
    env = Environment(
        loader=PackageLoader('felesatra', 'templates'),
        trim_blocks=True,
        lstrip_blocks=True,
        auto_reload=False)
    env.filters.update(filters.filters)
    return env
