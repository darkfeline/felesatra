"""Make Jinja environment."""

from jinja2 import Environment, PackageLoader

from . import filters


def make_env(env_globals):
    """Get Jinja environment."""
    env = Environment(
        loader=PackageLoader('frelia', 'templates'),
        trim_blocks=True,
        lstrip_blocks=True,
        auto_reload=False)
    env.filters.update(filters.filters)
    env.globals = env_globals.copy()
    return env
