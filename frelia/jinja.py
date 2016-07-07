"""Jinja extensions for frelia."""

import jinja2

import frelia.filters


class Environment(jinja2.Environment):

    """Custom Jinja environment for frelia."""

    DEFAULT_OPTIONS = {
        'trim_blocks': True,
        'lstrip_blocks': True,
        'auto_reload': False,
    }

    def __init__(self, **options):
        options = self._customize_options(options)
        super().__init__(**options)
        filters = self._load_filters_from_module(frelia.filters)
        self.filters.update(filters)

    @classmethod
    def _customize_options(cls, options):
        """Update default options with given custom options."""
        custom_options = cls.DEFAULT_OPTIONS.copy()
        custom_options.update(options)
        return custom_options

    @staticmethod
    def _load_filters_from_module(module):
        return {
            name: getattr(module, name)
            for name in module.__all__
        }
