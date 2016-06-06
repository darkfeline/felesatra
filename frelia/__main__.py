"""Frelia static site generator.

Dance!

"""

import argparse
import logging

import frelia.env
from frelia import page
from frelia import static

STATIC_FILES = 'static'
PAGES = 'pages'


def main():
    logging.basicConfig(level='DEBUG')
    args = parse_args()
    static.link_static_files(STATIC_FILES, args.build_dir)
    env = make_env(args)

    pages = list(page.load_pages(PAGES))
    page.build_pages(env, pages)

    # Load site.
    # Render site.


def parse_args():
    """Parse arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--site-url', default='https://www.felesatra.moe')
    return parser.parse_args()


def make_env(args):
    """Make Jinja environment."""
    env_globals = make_env_globals(args)
    return frelia.env.make_env(env_globals)


def make_env_globals(args):
    """Make globals for Jinja environment."""
    return {
        'site': {
            'url': args.site_url,
        },
        'build_dir': args.build_dir,
    }

if __name__ == '__main__':
    main()
