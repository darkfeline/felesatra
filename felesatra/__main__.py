"""Frelia static site generator."""

import argparse
import functools
import logging

import jinja2
import yaml

import frelia.enja
import frelia.jinja
import frelia.page
import frelia.fs


def main():
    """Dance!"""
    logging.basicConfig(level='DEBUG')
    args = parse_args()
    make_env = EnvironmentMaker(
        frelia.jinja.Environment,
        loader=jinja2.FileSystemLoader(args.template_dir))
    globals_dict = load_globals(args.globals)

    frelia.fs.link_files(args.static_dir, args.build_dir)

    env = make_env(globals_dict)
    pages = list(load_pages(args.page_dir))
    render_pages(env, pages, args.build_dir)


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


def render_pages(env, pages, build_dir):
    document_renderer = frelia.page.JinjaDocumentRenderer(env)
    page_renderer = frelia.page.PageRenderer(document_renderer, build_dir)
    for page in pages:
        page_renderer.render(page)


class EnvironmentMaker:

    def __init__(self, env_class, **kwargs):
        self.env_class = env_class
        self.kwargs = kwargs

    def __call__(self, globals_dict):
        env = self.env_class(**self.kwargs)
        env.globals = globals_dict
        return env


if __name__ == '__main__':
    main()
