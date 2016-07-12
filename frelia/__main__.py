"""Frelia static site generator."""

import argparse
import logging

import yaml

import frelia.jinja
import frelia.page
import frelia.fs


def main():
    """Dance!"""
    logging.basicConfig(level='DEBUG')
    args = parse_args()
    frelia.fs.link_files(args.static_dir, args.build_dir)
    globals_dict = load_globals(args.globals)

    content_pages = list(load_pages(env_globals, args.pages_dir))
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
    parser.add_argument('--page-dir', default='pages')
    parser.add_argument('--template-dir', default='templates')
    parser.add_argument('--globals', default='globals.yaml')
    return parser.parse_args()


def load_globals(filename):
    with open(filename) as file:
        return yaml.load(file, Loader=yaml.CLoader)


def load_pages(globals_dict, pages_dir):
    """Load content pages."""
    page_loader = ...
    loader = frelia.page.PageResourceLoader(
        frelia.page.PageLoader(),
        frelia.page.PageResource)
    env = frelia.jinja.Environment()
    env.globals = globals_dict
    yield from frelia.page.load_pages(env, pages_dir)


def build_pages(pages, build_dir):
    for page in pages:
        page.build(build_dir)


if __name__ == '__main__':
    main()
