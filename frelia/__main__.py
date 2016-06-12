"""Frelia static site generator.

Dance!

"""

from collections import OrderedDict
import argparse
import logging

import frelia.jinja
import frelia.page
import frelia.static

STATIC_FILES = 'static'
PAGES_DIR = 'pages'


def main():
    logging.basicConfig(level='DEBUG')
    args = parse_args()
    frelia.static.link_static_files(STATIC_FILES, args.build_dir)

    env_globals = make_env_globals(args)
    env = frelia.jinja.Environment(env_globals)
    pages = list(frelia.page.load_pages(env, PAGES_DIR))
    build_dir = args.build_dir
    for page in pages:
        page.build(build_dir)

    env_globals['site']['pages'] = pages
    env = frelia.jinja.Environment(env_globals)
    # Load site.
    # Render site.


def parse_args():
    """Parse arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--site-url', default='https://www.felesatra.moe')
    return parser.parse_args()


def make_env_globals(args):
    """Make globals for Jinja environment."""
    return {
        'site': {
            'nav': OrderedDict((
                ('About', 'about/'),
                ('Projects', 'projects/'),
            )),
            'url': args.site_url,
        },
        'build_dir': args.build_dir,
    }


if __name__ == '__main__':
    main()
