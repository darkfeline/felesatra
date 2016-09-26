"""Configuration loading."""

import io

import yaml


_DEFAULT = {
    'site_url': 'https://www.felesatra.moe',
    'static_dir': 'static',
    'page_dir': 'pages',
    'template_dir': 'templates',
    'globals': 'globals.yaml',
}


def load_config(file: io.TextIOBase):
    """Load YAML config from file."""
    config = _DEFAULT.copy()
    loaded_config = yaml.load(file, Loader=yaml.CLoader)
    config.update(loaded_config)
    return config
