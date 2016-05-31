import argparse
import logging

from .static import copy_static_files

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

if __name__ == '__main__':
    main()
