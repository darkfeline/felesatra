"""Frelia static site generator."""

from collections import OrderedDict
import argparse
import logging

import frelia.jinja
import frelia.page
import frelia.fs


def main():
    """Dance!"""
    logging.basicConfig(level='DEBUG')
    args = parse_args()

    frelia.fs.link_files(args.static_dir, args.build_dir)

    env_globals = make_env_globals(args)
    content_pages = list(load_content_pages(env_globals, args.content_pages_dir))
    build_pages(content_pages, args.build_dir)

    env_globals['site']['content_pages'] = content_pages
    env = frelia.jinja.Environment(env_globals)
    # Load site.
    # Render site.


def parse_args():
    """Parse arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--site-url', default='https://www.felesatra.moe')
    parser.add_argument('--static-dir', default='static')
    parser.add_argument('--content-page-dir', default='pages')
    parser.add_argument('--template-dir', default='templates')
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
        'build': {
            'build_dir': args.build_dir,
        },
    }


def load_content_pages(env_globals, pages_dir):
    """Load content pages."""
    env = frelia.jinja.Environment(env_globals)
    yield from frelia.page.load_pages(env, pages_dir)


def build_pages(pages, build_dir):
    for page in pages:
        page.build(build_dir)


if __name__ == '__main__':
    main()
