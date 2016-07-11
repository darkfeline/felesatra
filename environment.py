"""Jinja environment for frelia."""

import jinja2

import frelia.jinja


class Environment(frelia.jinja.Environment):

    """Custom Jinja environment for frelia."""

    DEFAULT_OPTIONS = {
        'loader': jinja2.PackageLoader('frelia', 'templates'),
    }

    def __init__(self, globals_dict, **options):
        options, param_options = self.DEFAULT_OPTIONS.copy(), options
        options.update(param_options)
        super().__init__(**options)
        self.globals = globals_dict.copy()
