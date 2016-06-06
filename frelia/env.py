"""Make Jinja environment."""

from collections import OrderedDict

from jinja2 import Environment, PackageLoader

from . import filters


def make_env(env_globals):
    """Get Jinja environment."""
    env = Environment(
        loader=PackageLoader('felesatra', 'templates'),
        trim_blocks=True,
        lstrip_blocks=True,
        auto_reload=False)
    env.filters.update(filters.filters)
    env_globals = env_globals.copy()
    env.globals = env_globals
    env_globals.setdefault('site', {})['nav'] = OrderedDict((
        ('About', 'about/'),
        ('Projects', 'projects/'),
    ))
    return env
