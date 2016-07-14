"""Frelia static site generator."""

import argparse
import logging

import coloredlogs
import frelia.document.enja as enja
import frelia.document.renderers as document_renderers
import frelia.fs
import frelia.jinja
import frelia.page
import frelia.transforms.document as document_transforms
import frelia.transforms.generic as generic_transforms
import frelia.transforms.page as page_transforms
import jinja2
import yaml

import felesatra.transforms

logger = logging.getLogger(__name__)


def main():
    """Dance!"""
    coloredlogs.install(
        level='DEBUG',
        fmt='%(asctime)s %(name)s %(levelname)s %(message)s')
    args = parse_args()
    make_env = EnvironmentMaker(
        frelia.jinja.Environment,
        loader=jinja2.FileSystemLoader(args.template_dir),
        extensions=['jinja2.ext.with_'])
    globals_dict = load_globals(args.globals)
    globals_dict['site']['url'] = args.site_url

    logger.info('Linking static files...')
    frelia.fs.link_files(args.static_dir, args.build_dir)

    logger.info('Loading pages...')
    pages = list(load_pages(args.page_dir))

    logger.info('Preprocessing pages...')
    preprocess_pages(pages, args.page_dir)
    aggregation_pages = [page for page in pages if is_aggregation(page)]
    content_pages = [page for page in pages if not is_aggregation(page)]

    logger.info('Processing pages...')
    env = make_env(globals_dict)
    process_pages(env, content_pages, args.build_dir)

    globals_dict['site']['pages'] = content_pages

    logger.info('Processing aggregation pages...')
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
    page_loader = frelia.page.PageLoader(enja.read)
    yield from page_loader.load_pages(page_dir)


def preprocess_pages(pages, basepath):
    """Do preliminary page transformations."""
    transform = generic_transforms.ComposeTransforms([
        page_transforms.RebasePagePath(basepath),
        page_transforms.strip_page_extension,
        page_transforms.DateFromPath('published'),
        page_transforms.DocumentPageTransforms([
            felesatra.transforms.set_updated_from_published,
            felesatra.transforms.mark_aggregations,
            document_transforms.SetDefaultMetadata({
                'aggregate': True,
            }),
        ]),
    ])
    transform(pages)


def process_pages(env, pages, build_dir):
    """Transform, render, and write pages."""
    transform_pages(env, pages)
    render_pages(env, pages)
    write_pages(pages, build_dir)


def transform_pages(env, pages):
    transform = page_transforms.DocumentPageTransforms([
        document_transforms.RenderJinja(env),
    ])
    transform(pages)


def render_pages(env, pages):
    document_renderer = document_renderers.JinjaDocumentRenderer(env)
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
