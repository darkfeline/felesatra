"""Make Jinja environment."""

from collections import OrderedDict

from jinja2 import Environment, PackageLoader

from felesatra import filters


def make_env(site_url):
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
            'nav': OrderedDict((
                ('Keihan', 'keihan/'),
            )),
        },
    }
    return env
