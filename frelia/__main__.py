# pylint: disable=missing-docstring

import argparse
import logging
import os

STATIC_FILES = 'static'


def main():
    logging.basicConfig(level='DEBUG')

    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--site-url', default='https://www.felesatra.moe')
    args = parser.parse_args()

    copy_static_files(STATIC_FILES, args.build_dir)
    # Load site.
    # Load pages.
    # Render pages.
    # Render site.


def copy_static_files(src, dst):
    for src_path in walk_files(src):
        rel_path = os.path.relpath(src_path)
        dst_path = os.path.join(dst, rel_path)
        os.makedirs(os.path.dirname(dst_path), exist_ok=True)
        os.link(src_path, dst_path)


def walk_files(path):
    for dirpath, dirnames, filenames in os.walk(path):  # pylint: disable=unused-variable
        for filename in filenames:
            yield os.path.join(dirpath, filename)

if __name__ == '__main__':
    main()
