"""Frelia static site generator."""

import argparse
import logging

import frelia.config
import frelia.jinja
import frelia.page
import frelia.fs


def main():
    """Dance!"""
    logging.basicConfig(level='DEBUG')
    args = parse_args()
    config = load_config(args.config)

    frelia.fs.link_files(config['static_dir'], config['build_dir'])

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
    parser.add_argument('--config', default='config.yaml')
    return parser.parse_args()


def load_config(filename):
    with open(filename) as file:
        return frelia.config.load(file)


def load_content_pages(env_globals, pages_dir):
    """Load content pages."""
    env = frelia.jinja.Environment(env_globals)
    yield from frelia.page.load_pages(env, pages_dir)


def build_pages(pages, build_dir):
    for page in pages:
        page.build(build_dir)


if __name__ == '__main__':
    main()
