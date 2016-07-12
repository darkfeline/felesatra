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


class Config(dict):

    @classmethod
    def load(cls, file):
        return yaml.load(file, Loader=yaml.CLoader)
