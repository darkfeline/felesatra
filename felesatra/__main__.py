"""Frelia static site generator."""

import argparse
import logging

import coloredlogs
import frelia.jinja
import frelia.transforms.page as page_transforms
import jinja2

import felesatra.build

logger = logging.getLogger(__name__)


def main():
    coloredlogs.install(
        level='DEBUG',
        fmt='%(asctime)s %(name)s %(levelname)s %(message)s')
    args = parse_args()

    logger.info('Linking static files...')
    felesatra.build.link_static_files(args.static_dir, args.build_dir)

    make_env = felesatra.build.EnvironmentMaker(
        frelia.jinja.Environment,
        loader=jinja2.FileSystemLoader(args.template_dir),
        extensions=['jinja2.ext.with_'])

    globals_dict = felesatra.build.load_globals_dict(args.globals)
    globals_dict['site']['url'] = args.site_url

    logger.info('Loading pages...')
    pages = felesatra.build.load_pages(args.page_dir)
    rebase_paths = page_transforms.RebasePagePath(args.page_dir)
    rebase_paths(pages)

    logger.info('Building site...')
    builder = felesatra.build.BuildProcess(args.build_dir, make_env)
    builder(globals_dict, pages)


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


if __name__ == '__main__':
    main()
