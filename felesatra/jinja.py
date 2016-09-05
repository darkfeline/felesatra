"""Custom Jinja Environment class."""

import jinja2

import frelia.filters


class Environment(jinja2.Environment):

    """Custom Jinja Environment subclass.

    This subclass sets better defaults and includes custom filters from
    frelia.filters.

    """

    __DEFAULT_OPTIONS = {
        'trim_blocks': True,
        'lstrip_blocks': True,
        'auto_reload': False,
        'loader': jinja2.PackageLoader('frelia', 'templates'),
        'bytecode_cache': jinja2.FileSystemBytecodeCache(),
    }

    def __init__(self, **options):
        options = self._customize_options(options)
        super().__init__(**options)
        filters = _load_filters_from_module(frelia.filters)
        self.filters.update(filters)

    @classmethod
    def _copy_default_options(cls):
        return cls.__DEFAULT_OPTIONS.copy()

    @classmethod
    def _customize_options(cls, options):
        """Update default options with given custom options."""
        custom_options = cls._copy_default_options()
        custom_options.update(options)
        return custom_options


def _load_filters_from_module(module):
    """Load filters from a filter module.

    A filter module should include its filter functions in its __all__
    attribute.

    Returns a dict suitable for updating a Jinja Environment's filters.

    """
    return {
        name: getattr(module, name)
        for name in module.__all__
    }
