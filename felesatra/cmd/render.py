"""Render webpages."""

import argparse
import logging
import os
import sys

import jinja2

from felesatra import enja

logger = logging.getLogger(__name__)


def main(argv):
    logging.basicConfig(level='INFO')
    parser = argparse.ArgumentParser(prog=argv[0], description=__doc__)
    parser.add_argument('src')
    parser.add_argument('dst')
    args = parser.parse_args(argv[1:])
    _render_recursively(args.src, args.dst)
    return 0


def _render_recursively(src: str, dst: str):
    env = _make_env()
    for root, dirs, files in os.walk(src):
        for file in files:
            srcfile = os.path.join(root, file)
            relpath = os.path.relpath(srcfile, src)
            dstfile = os.path.join(dst, relpath)
            logger.info('Rendering %s to %s', srcfile, dstfile)
            os.makedirs(os.path.dirname(dstfile), exist_ok=True)
            _render_document(env, srcfile, dstfile)


def _render_document(env, src: str, dst: str):
    with open(src) as f:
        document = enja.load(f)
    template = _document_template(env, document)
    context = enja.context(document, src)
    html = template.render(context)
    with open(dst, 'w') as f:
        f.write(html)


def _make_env():
    env = jinja2.Environment(
        loader=jinja2.PackageLoader('felesatra', 'templates'),
        trim_blocks=True,
        lstrip_blocks=True,
        auto_reload=False)
    return env


def _document_template(env, document):
    return env.get_template(document.header.get('template', 'content_page.html'))


if __name__ == '__main__':
    sys.exit(main(sys.argv))
