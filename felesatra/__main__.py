"""Frelia static site generator."""

import argparse
import logging

import frelia.document.enja
import frelia.document.renderers
import frelia.fs
import frelia.jinja
import frelia.page
import frelia.transform
import jinja2
import yaml


def main():
    """Dance!"""
    logging.basicConfig(level='DEBUG')
    args = parse_args()
    make_env = EnvironmentMaker(
        frelia.jinja.Environment,
        loader=jinja2.FileSystemLoader(args.template_dir))
    globals_dict = load_globals(args.globals)
    globals_dict['site']['url'] = args.site_url

    frelia.fs.link_files(args.static_dir, args.build_dir)

    pages = list(load_pages(args.page_dir))
    preprocess_pages(pages, args.page_dir)
    aggregation_pages = [page for page in pages if is_aggregation(page)]
    content_pages = [page for page in pages if not is_aggregation(page)]

    env = make_env(globals_dict)
    process_pages(env, content_pages, args.build_dir)

    globals_dict['site']['pages'] = content_pages

    env = make_env(globals_dict)
    process_pages(env, aggregation_pages, args.build_dir)


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


class EnvironmentMaker:

    def __init__(self, env_class, **kwargs):
        self.env_class = env_class
        self.kwargs = kwargs

    def __call__(self, globals_dict=None):
        env = self.env_class(**self.kwargs)
        if globals_dict is not None:
            env.globals = globals_dict
        return env


def load_globals(filename):
    """Load globals dict from YAML file."""
    with open(filename) as file:
        return yaml.load(file, Loader=yaml.CLoader)


def load_pages(page_dir):
    page_loader = frelia.page.PageLoader(frelia.document.enja.read)
    yield from page_loader.load_pages(page_dir)


def process_pages(env, pages, build_dir):
    """Transform, render, and write pages."""
    transform_pages(env, pages)
    render_pages(env, pages)
    write_pages(pages, build_dir)


def preprocess_pages(pages, basepath):
    transform = frelia.transform.TransformGroup([
        frelia.transform.RebasePagePath(basepath),
        frelia.transform.strip_page_extension,
    ])
    transform(pages)


def transform_pages(env, pages):
    transform = frelia.transform.TransformGroup([
        frelia.transform.DocumentPageTransform(
            frelia.transform.RenderJinja(env)),
    ])
    transform(pages)


def render_pages(env, pages):
    document_renderer = frelia.document.renderers.JinjaDocumentRenderer(env)
    page_renderer = frelia.page.PageRenderer(document_renderer)
    page_renderer(pages)


def write_pages(pages, build_dir):
    writer = frelia.page.PageWriter(build_dir)
    writer(pages)


def is_aggregation(page):
    """Return True if page is an aggregation page."""
    return page.document.metadata.get('aggregation', False)


if __name__ == '__main__':
    main()
