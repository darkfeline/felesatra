"""Make Jinja environment."""

from collections import OrderedDict

from jinja2 import Environment, PackageLoader

from . import filters


def make_env(site_url, src_dir):
    """Get Jinja environment."""
    env = Environment(
        loader=PackageLoader('felesatra', 'templates'),
        trim_blocks=True,
        lstrip_blocks=True,
        auto_reload=False)
    env.filters = filters.filters
    env.globals = {
        'site': {
            'url': site_url,
            'srcdir': src_dir,
            'nav': OrderedDict((
                ('About', 'about/'),
                ('Projects', 'projects/'),
            )),
        },
        'sitemap': [
        ],
    }
    return env
