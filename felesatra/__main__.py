"""Frelia static site generator."""

import argparse
import logging

import yaml

import frelia.enja
import frelia.jinja
import frelia.page
import frelia.fs


def main():
    """Dance!"""
    logging.basicConfig(level='DEBUG')
    args = parse_args()
    frelia.fs.link_files(args.static_dir, args.build_dir)
    globals_dict = load_globals(args.globals)
    pages = list(load_pages(args.page_dir))
    render_pages(globals_dict, pages, args.build_dir)


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


def load_pages(page_dir):
    page_loader = frelia.page.PageLoader(frelia.enja.EnjaDocument)
    yield from page_loader.load_pages(page_dir)


def render_pages(globals_dict, pages, build_dir):
    env = frelia.jinja.Environment()
    env.globals = globals_dict
    document_renderer = frelia.page.JinjaDocumentRenderer(env)
    page_renderer = frelia.page.PageRenderer(document_renderer, build_dir)
    for page in pages:
        page_renderer.render(page)


if __name__ == '__main__':
    main()
