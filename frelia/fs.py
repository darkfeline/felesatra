"""File system utilities."""

import os


def walk_files(path):
    """Generate the pathnames of all files in a directory tree."""
    for dirpath, dirnames, filenames in os.walk(path):  # pylint: disable=unused-variable
        for filename in filenames:
            yield os.path.join(dirpath, filename)
