"""Configuration loading for building frelia sites."""

import copy

import yaml


class _AttrDict:

    """dict wrapper that allows lookup by attribute."""

    def __new__(cls, value):
        if isinstance(value, dict):
            return super().__new__(cls)
        else:
            return value

    def __init__(self, dict_):
        self.__dict = dict_

    def __eq__(self, other):
        return self.__dict == other

    def __getattr__(self, name):
        return type(self)(self.__dict[name])


def _recursive_update(base, other):
    """Recursively update dictionaries."""
    for key, value in other.items():
        recurse = (
            key in base
            and isinstance(base[key], dict)
            and isinstance(value, dict)
        )
        if recurse:
            _recursive_update(base[key], value)
        else:
            base[key] = value


DEFAULT_CONFIG = {
    'site_url': 'https://www.felesatra.moe',
    'static_dir': 'static',
    'pages_dir': 'pages',
    'template_dir': 'templates',
}


def load(file):
    """Load configuration from YAML file with defaults.

    The default configuration is loaded into the 'config' section, leaving the
    namespace available for user settings.  frelia-specific configuration will
    go into the 'config' section.

    """
    config = {
        'config': copy.deepcopy(DEFAULT_CONFIG),
    }
    user_config = yaml.load(file, Loader=yaml.CLoader)
    _recursive_update(config, user_config)
    return _AttrDict(config)
