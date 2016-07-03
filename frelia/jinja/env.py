"""Jinja environment for frelia."""

import jinja2

from . import filters


class Environment(jinja2.Environment):

    """Custom Jinja environment for frelia."""

    DEFAULT_OPTIONS = {
        'loader': jinja2.PackageLoader('frelia', 'templates'),
        'trim_blocks': True,
        'lstrip_blocks': True,
        'auto_reload': False,
    }

    def __init__(self, globals_dict, **options):
        options, param_options = self.DEFAULT_OPTIONS.copy(), options
        options.update(param_options)
        super().__init__(**options)
        self.filters.update(filters.filters)
        self.globals = globals_dict.copy()
