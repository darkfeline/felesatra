"""Jinja extensions for frelia."""

import jinja2
from jinja2 import Environment, PackageLoader

from . import filters


class Environment(jinja2.Environment):

    def __init__(self, globals_dict, **options):
        final_options = self._default_options
        final_options.update(options)
        super().__init__(**final_options)
        self.filters.update(filters.filters)
        self.globals = globals_dict.copy()

    @property
    def _default_options(self):
        return {
            'loader': PackageLoader('frelia', 'templates'),
            'trim_blocks': True,
            'lstrip_blocks': True,
            'auto_reload': False,
        }
