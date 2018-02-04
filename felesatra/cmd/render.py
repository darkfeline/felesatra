"""Render a webpage from an Enja document."""

import argparse
import datetime
import pathlib
import sys

import jinja2

from felesatra import enja


def main(argv):
    parser = argparse.ArgumentParser(prog=argv[0],
                                     description=__doc__)
    parser.add_argument('src')
    parser.add_argument('dst')
    args = parser.parse_args(argv[1:])
    _render_document(args.src, args.dst)
    return 0


def _render_document(src: str, dst: str):
    env = _make_env()
    with open(src) as f:
        document = enja.load(f)
    template = _document_template(env, document)
    context = _document_context(document, src)
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


def _document_context(document, src: str):
    context = document.header.copy()
    context['content'] = document.body
    if 'published' not in context:
        context['published'] = _parse_published(src)
    if 'modified' not in context:
        context['modified'] = context['published']
    return context


def _parse_published(path: str):
    path = pathlib.PurePath(path)
    try:
        y, m, d = path.parts[-3:]
    except ValueError:
        return datetime.datetime.min
    try:
        return datetime.date(int(y), int(m), int(d))
    except ValueError:
        return datetime.datetime.min


if __name__ == '__main__':
    sys.exit(main(sys.argv))
