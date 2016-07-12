import copy

import yaml


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
    """Load configuration from YAML file with defaults."""
    config = copy.deepcopy(DEFAULT_CONFIG)
    user_config = yaml.load(file, Loader=yaml.CLoader)
    _recursive_update(config, user_config)
    return config
