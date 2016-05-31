import os


def walk_files(path):
    for dirpath, dirnames, filenames in os.walk(path):  # pylint: disable=unused-variable
        for filename in filenames:
            yield os.path.join(dirpath, filename)
