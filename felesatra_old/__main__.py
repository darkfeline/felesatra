import argparse
import logging
import os
import pathlib
from collections import OrderedDict
from urllib.parse import urljoin

from felesatra.env import make_env
from felesatra.resource.index import Author, Link
import felesatra.resource.loaders
import felesatra.resource.resources


def sitepath():
    """Get path of website sources."""
    return os.path.join(
        os.path.dirname(__file__),
        'site',
    )


def setup_env(env, site_url, src_dir):
    """Set up environment."""
    site_url = urljoin(site_url, '/')
    env.globals['site'] = {
        'url': site_url,
        'srcdir': src_dir,
        'nav': OrderedDict((
            ('About', 'about/'),
            ('Projects', 'projects/'),
        )),
    }
    self_link = Link(urljoin(site_url, 'atom.xml'))
    self_link.rel = 'self'
    self_link.type = 'application/atom+xml'
    env.globals['atom_context'] = {
        'id': site_url,
        'title': 'Feles Atra',
        'links': [self_link],
        'authors': [Author('Allen Li', site_url, None)],
        'rights': 'Copyright 2010-2016 Allen Li',
    }


def main():
    logging.basicConfig(level='DEBUG')

    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--site-url', default='https://www.felesatra.moe')
    args = parser.parse_args()

    src_dir = sitepath()
    env = make_env()
    setup_env(env, args.site_url, src_dir)

    args.build_dir = pathlib.Path(args.build_dir)

    loader = felesatra.resource.loaders.WebsiteLoader()
    resources = list(loader(src_dir))

    index_entries = [
        resource.get_index_entry(env)
        for resource in resources
        if isinstance(resource, felesatra.resource.resources.Indexable)
    ]
    env.globals['page_index'] = index_entries

    for resource in resources:
        resource.deploy(env, args.build_dir)
    sitemap = felesatra.resource.resources.SitemapResource('sitemap.xml')
    sitemap.deploy(env, args.build_dir)
    if 'atom_context' in env.globals:
        atom = felesatra.resource.resources.AtomResource(
            env.globals['atom_context'], 'atom.xml')
        atom.deploy(env, args.build_dir)


if __name__ == '__main__':
    main()
